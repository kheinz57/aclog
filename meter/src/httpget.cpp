/*
  Copyright 2010 Heinz Haeberle

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
//#define USESSL

#include <iostream>
#include <istream>
#include <ostream>
#include <boost/asio.hpp>
#ifdef USESSL
#include <boost/asio/ssl.hpp>
#endif
#include "httpget.h"

using boost::asio::ip::tcp;


int httpget(int argc, char* argv[])
{
    if (argc < 4)
    {
      std::cout << "Usage: sync_client <server> <port> <path>\n";
      std::cout << "Example:\n";
      std::cout << "  sync_client www.boost.org 80 /LICENSE_1_0.txt\n";
      return 1;
    }
	std::string server(argv[1]);
	std::string port(argv[2]);
	std::string path(argv[3]);
	return httpgets(server,port,path);
}

int httpgets(std::string server, std::string port, std::string path)
{
	try
	{
		boost::asio::io_service io_service;

		// Get a list of endpoints corresponding to the server name.
		tcp::resolver resolver(io_service);
		tcp::resolver::query query(server, port);
		tcp::resolver::iterator endpoint_iterator = resolver.resolve(query);
		tcp::resolver::iterator end;
#ifdef USESSL
		boost::asio::ssl::context ctx(io_service, boost::asio::ssl::context::sslv3);
		ctx.set_verify_mode(boost::asio::ssl::context::verify_peer);
		ctx.load_verify_file("ssl.crt");
		boost::asio::ssl::stream<boost::asio::ip::tcp::socket> socket(io_service,ctx);
#else
		tcp::socket socket(io_service);
#endif
		// Try each endpoint until we successfully establish a connection.
		boost::system::error_code error = boost::asio::error::host_not_found;
		while (error && endpoint_iterator != end)
		{
#ifdef USESSL
		  socket.lowest_layer().close();
		  socket.lowest_layer().connect(*endpoint_iterator++, error);
#else
		  socket.close();
		  socket.connect(*endpoint_iterator++, error);
#endif
		}
		if (error)
		  throw boost::system::system_error(error);

		// Form the request. We specify the "Connection: close" header so that the
		// server will close the socket after transmitting the response. This will
		// allow us to treat all data up until the EOF as the content.
		boost::asio::streambuf request;
		std::ostream request_stream(&request);
		request_stream << "GET " << path << " HTTP/1.0\r\n";
		request_stream << "Host: " << server << "\r\n";
		request_stream << "Accept: */*\r\n";
		request_stream << "Connection: close\r\n\r\n";

		// Send the request.
		boost::asio::write(socket, request);

		// Read the response status line.
		boost::asio::streambuf response;
		boost::asio::read_until(socket, response, "\r\n");

		// Check that response is OK.
		std::istream response_stream(&response);
		std::string http_version;
		response_stream >> http_version;
		unsigned int status_code;
		response_stream >> status_code;
		std::string status_message;
		std::getline(response_stream, status_message);
		if (!response_stream || http_version.substr(0, 5) != "HTTP/")
		{
		  std::cout << "Invalid response\n";
		  return 1;
		}
		if (status_code != 200)
		{
		  std::cout << "Response returned with status code " << status_code << "\n";
		  return 1;
		}

		// Read the response headers, which are terminated by a blank line.
		boost::asio::read_until(socket, response, "\r\n\r\n");

		// Process the response headers.
		std::string header;
		while (std::getline(response_stream, header) && header != "\r")
		  std::cout << header << "\n";
		std::cout << "\n";

		// Write whatever content we already have to output.
		if (response.size() > 0)
		  std::cout << &response;

		// Read until EOF, writing data to output as we go.
		while (boost::asio::read(socket, response,
			  boost::asio::transfer_at_least(1), error))
		  std::cout << &response;
		if (error != boost::asio::error::eof)
		  throw boost::system::system_error(error);
	}
	catch (std::exception& e)
	{
		std::cout << "Exception: " << e.what() << "\n";
		return 1;
	}

	return 0;
}
