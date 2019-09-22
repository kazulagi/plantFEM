var net=require('net');

net.createServer(function (socket){
    socket.write('This is RLS Tech News\r\n');
    socket.on('data',function(data){
        socket.write(data);
    });
}).listen(1337,'127.0.0.1');