hs-downloader
=============

Multipart threaded file downloader with counduits

## Usage

	runhaskell Download.hs <url>
	

This will start 5 threads each downloading a range of bytes.  
When all the parts will be download the 5 part files will be joined in a final one.

## TODO
* Handle Exceptions
* Command line arguments
* Better progress bar
* Lots of stuff