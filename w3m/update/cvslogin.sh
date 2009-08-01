#!/usr/bin/expect
send "cd /home/agha/j-waldby/yume\r"
spawn cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
expect password:
send "\r"
send_user "\r"
exit
# expect -c "
# spawn cvs -d :pserver:anonymous at www.cmake.org:/cvsroot/CMake login;
# expect \"CVS password:\";
# send \"cmake\r\";
# send_user \"\r\nDone CVS login\r\n\";
# exit"