ALL = subcmds readme opts

all: $(ALL)

subcmds: subcmds.hs
	ghc subcmds.hs

readme: readme.hs
	ghc readme.hs

opts: opts.hs
	ghc opts.hs

clean:
	rm $(ALL)
