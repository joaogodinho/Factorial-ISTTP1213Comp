all:	
	$(MAKE) -C solex
	$(MAKE) -C lib
	$(MAKE) -C interm
	#$(MAKE) -C exs
	cp interm/factorial .

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C solex clean
	$(MAKE) -C interm clean
	#$(MAKE) -C exs clean
	rm factorial
