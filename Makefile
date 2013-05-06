all:	
	$(MAKE) -C solex
	$(MAKE) -C lib
	#$(MAKE) -C interm
	$(MAKE) -C final
	#$(MAKE) -C exs
	cp final/factorial .

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C solex clean
	#$(MAKE) -C interm clean
	$(MAKE) -C final clean
	#$(MAKE) -C exs clean
	rm factorial
