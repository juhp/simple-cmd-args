ALL = subcmds readme opts subcmds2

all: $(ALL)

subcmds: subcmds.hs
	ghc -package simple-cmd subcmds.hs

readme: readme.hs
	ghc readme.hs

opts: opts.hs
	ghc opts.hs

subcmds2: subcmds2.hs
	ghc subcmds2.hs

clean:
	rm $(ALL)
