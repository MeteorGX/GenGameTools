-sname __PROJECT___1 # 启动分布式, 多服就按照 __PROJECT___1,__PROJECT___2,... 分服, node() 查看节点
-setcookie __PROJECT___cookie # 节点会话标识
-smp enable # 支持SMP调度

+K true # 启用异步池
+pc unicode # 启用多语言支持
+P 102400 # 最大进程数
+A 30 # 异步池数量
+e 102400 # ETS最大数量