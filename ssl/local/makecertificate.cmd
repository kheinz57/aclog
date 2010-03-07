c:\Programme\OpenSSL\bin\openssl genrsa -des3 -out server.key 1024
c:\Programme\OpenSSL\bin\openssl req -new -key server.key -out server.csr
copy server.key server.key.org
c:\Programme\OpenSSL\bin\openssl rsa -in server.key.org -out server.key
c:\Programme\OpenSSL\bin\openssl x509 -req -days 1000 -in server.csr -signkey server.key -out server.crt
cp server.crt ..\..\erlangserver\ssl.crt
cp server.key ..\..\erlangserver\ssl.key