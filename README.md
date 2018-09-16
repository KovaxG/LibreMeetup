# LibreMeetup
This program was written using the Haskell language and was compiled with the   Glasgow Haskell Compiler. I made this little program for fun, and did not care   about proper standards at all.    If you wish to use this code, or modify it, please do so.

**Disclaimer: This is not a scalable or good solution! It is really easy to take down this server, and it is not intended to be used for commercial purposes. It was made just for fun.**

# Usage
There are a couple of steps if you want to run this program.  
1. First of all, you need to look up your local IP address. You can do that using the ipconfig console command in windows.  
You need to modify this line in the source file, and enter your own IP address:
`localIPAddress =  Host "192.168.0.xxx"`

2. In order to compile it into an executable, you need to run the build.bat script.  
This will create a server.exe.  

3. The third thing you need to do is to do a port forwarding. This is different for every router, so you will have to look that up.
