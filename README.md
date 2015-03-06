# doompiler
Welcome to the doompiler! 
This is a compiler project written in Haskell for [Alan Labouseur's Design of Compilers class](http://labouseur.com/courses/compilers/).

To run the compiler, you will need to install Haskell's compiler, GHC, and "package manager," cabal.
Both can be installed from one package called [Haskell Platform](https://www.haskell.org/platform/).

If you prefer to install them individually from [homebrew](http://brew.sh), you can do so by running:
```
brew install ghc
brew install cabal-install
```

Once you have installed Haskell and cloned this repo, you can run the compiler from the project directory by doing:
```
cabal build
cabal run < file.txt
```
where file.txt contains the program you would like to run.
