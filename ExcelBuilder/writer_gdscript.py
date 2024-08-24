#! python
# -*- coding: UTF-8 -*-

from writer import *


class GdscriptWriter(Writer):
    # 文件后缀
    def suffix(self):
        return ".gd"

    # 文件注释
    def comment(self):
        comment = [
            'class_name ' + self.base_name,
            'extends Object',
            '## Automatic generation from -->>',
            '## excel file  name: ' + self.doc_name,
            '## excel sheet name: ' + self.sheet_name
        ]
        return "\n".join(comment)

    # python的tuple转换为elixir的tuple类型
    def tuple_to_elixir(self, value, indent):
        tuple_ctx_list = []

        for v in value:
            is_indent, lv = self.to_target_lang(v, indent + 1)
            tuple_ctx_list.append(lv)

        # 返回 {a,b,c}这种不换行的格式
        list_str = ", ".join(tuple_ctx_list)
        return False, "".join(["[", list_str, "]"])

    # python的dict转换为elixir的map类型
    def dict_to_elixir(self, value, indent):
        dict_ctx_list = []

        for k in value:
            k_indent, lk = self.to_target_lang(k, indent)
            is_indent, lv = self.to_target_lang(value[k], indent + 1)

            val_type = type(lk)
            if str == val_type:
                lk = lk.replace("\"", "\'")
            key = "".join([lk, " : "])
            val = "".join([key, lv])
            dict_ctx_list.append(val)

        dict_str = ", ".join(dict_ctx_list)
        return False, "".join(["{", dict_str, "}"])

    # python的list转换为elixir的list类型
    def list_to_elixir(self, value, indent):
        list_ctx_list = []
        for v in value:
            is_indent, lv = self.to_target_lang(v, indent + 1)
            list_ctx_list.append(lv)

        list_str = ", ".join(list_ctx_list)
        return False, "".join(["[", list_str, "]"])

    # 变量转换到目标语言字符串
    def to_target_lang(self, value, indent):
        val_type = type(value)
        if int == val_type:
            return False, str(value)
        elif float == val_type:
            # 1001.0 -->> 001 去除多余小数点
            if int(value) == value:
                return False, str(int(value))
            return False, str(value)
        elif str == val_type:
            return False, "".join(["\"", value, "\""])
        elif tuple == val_type:
            return self.tuple_to_elixir(value, indent)
        elif dict == val_type:
            return self.dict_to_elixir(value, indent)
        elif list == val_type:
            return self.list_to_elixir(value, indent)
        else:
            raise Exception("invalid type", val_type)

    # 将 dict 数据转为 gdscript
    def dict_to_text(self, value, indent):
        dict_text_list = ["\n", "\n", "static func get_item(key):", "\n"]
        # 生成 get_item() 函数
        key_first = True
        for k in value:
            k_indent, lk = self.to_target_lang(k, indent)
            is_indent, lv = self.to_target_lang(value[k], indent + 1)
            comment = self.comment_text[k]

            # 确认首次
            if key_first:
                if_text = "if"
                key_first = False
            else:
                if_text = "elif"

            key = "".join(["	", if_text, " key == ", lk, ":\n		return "])
            val = "".join(["	## ", comment, "\n", key, lv, "\n"])
            dict_text_list.append(val)

        dict_str = "".join(dict_text_list)
        dict_str = dict_str + "	else:\n		return null\n"
        return False, dict_str

    # 将 list 数据转为 gdscript
    def list_to_text(self, value, indent):
        list_text_list = ["\n", "\n", "static func get_item(idx):", "\n"]
        # 生成 get_item() 函数
        all_key_list = []
        key_first = True

        for i, one_dict in enumerate(value):
            # 生成对应的 key
            key_list = []
            for k in one_dict:
                if not (None is self.keys_list.get(k, None)):
                    is_indent, lv = self.to_target_lang(one_dict[k], indent + 1)
                    key_list.append(lv)
            all_key_list.append(key_list)
            union_key = key_list[0]

            # 确认首次
            if key_first:
                if_text = "if"
                key_first = False
            else:
                if_text = "elif"

            key = "".join(["	", if_text, " idx == ", union_key, ":\n		return {", "\n"])

            # 生成对应的value
            value_list = []
            for k in one_dict:
                k_indent, lk = self.to_target_lang(k, indent)
                is_indent, lv = self.to_target_lang(one_dict[k], indent + 1)
                one_val = "".join(["			", lk, ":", lv])
                value_list.append(one_val)

            value_list_str = ",\n".join(value_list)
            end_str = "".join([key, value_list_str, "\n		}", "\n"])
            list_text_list.append(end_str)

        no_match_str = "".join(["	else:", "\n", "		return null", "\n", "\n"])
        list_text_list.append(no_match_str)

        # 生成 get_list() 函数
        get_all_fun = []
        for i, i_val in enumerate(all_key_list):
            one_val = ", ".join(i_val)
            get_all_fun.append("		" + one_val)
        value_list_str = ",\n".join(get_all_fun)
        start_str = 'static func get_list():\n	return [\n'
        end_str = "".join([start_str, value_list_str, "\n", "    ]"])
        list_text_list.append(end_str)
        dict_str = "".join(list_text_list)
        return False, dict_str

    # 转换为文本数据, 之前解析出来的excel数据存放方式存在LIST(array格式)和DICT(object格式)两种类型
    def to_text(self, value, indent):
        val_type = type(value)
        if dict == val_type:
            return self.dict_to_text(value, indent)
        else:
            return self.list_to_text(value, indent)

    def context(self, ctx):
        is_indent, str_ctx = self.to_text(ctx, 0)
        return "".join([self.comment(), "", str_ctx])
