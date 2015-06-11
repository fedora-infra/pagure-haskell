standalone-haddock --package-db /home/ricky/.ghc/x86_64-linux-7.8.4/package.conf.d/ -o doc . --hyperlink-source
rsync --delete --partial --progress -avzre ssh doc/ fedorapeople.org:public_html/pagure-haskell
rm -rf doc
