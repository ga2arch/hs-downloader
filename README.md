hs-downloader
=============

Multipart threaded file downloader with counduits

## Features
* Multipart threaded download
* Use conduits to not abuse memory
* Neat progressbar

## Installation

	git clone git@github.com:ga2arch/hs-downloader.git
	cd hs-downloader
	cabal install --prefix=$HOME --user
	
Now there will be an executable called **hdownload** in your home's bin.

## Usage

	hdownload <url>
	
## TODO
* Handle Exceptions
* Command line arguments
* Better progress bar
* Lots of stuff