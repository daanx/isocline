icversion=`grep -E version package.yaml`
icversion="${icversion##version:* }" # bash pattern syntax: "* " == [ ]*

echo "isocline version: v$icversion"

echo "generate distribution tar for hackage.."
cabal v2-sdist

echo "generate haddock documentation for hackage.."
mkdir -p build/hdoc
cabal v2-haddock --builddir=./build/hdoc --haddock-for-hackage --enable-doc

hdoctar="./build/hdoc/isocline-$icversion-docs.tar.gz"
echo "upload haddock documentation $hdoctar.."
cabal upload -d --publish $hdoctar