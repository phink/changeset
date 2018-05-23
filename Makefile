default:
	jbuilder build @install --dev

doc:
	jbuilder build @doc

test:
	jbuilder runtest

clean:
	jbuilder clean

.PHONY: default doc test clean
