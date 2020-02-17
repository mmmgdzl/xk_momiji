::执行编译
call compile.bat
::启动服务
start werl -pa ebin ../emysql/ebin -s xk_momiji_generator
::关闭批处理窗口
EXIT