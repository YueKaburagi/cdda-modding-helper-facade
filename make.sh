#!/bin/bash
# bash for pushd,popd

distSrc=resources/app
distName=cdda-modding-helper-facade
distIgnorePatterns="(.*~$|\\.#.*)"
distOutput=package

fMkdir () {
    mkdir -p resources/app/img
}
fCpElec () {
    cp -f src/elec/* resources/app
    cp -f src/img/* resources/app/img
}
fStyl () {
    stylus src/styl/window.styl -o "resources/app/window.css"
}
fPurs () {
    pulp build --src-path "src/purs" --test-path "test/purs" -o "resources/app/output"
}
fMods () {
    pushd resources/app
    npm install
    popd
}
fPackage () {
    npm run packager -- $distSrc $distName --platform=$1 --arch=ia32,x64 \
		      --out=$distOutput --overwrite=true --ignore=$distIgnorePatterns
}
fPackageAll () {
    fPackage win32,darwin,linux
}
fPackageLin () {
    fPackage linux
}
fZip () {
    zip -r `basename $1` $1
}
fZipAll () {
    pushd $distOutput
    for x in `ls .`; do
	fZip $x
    done
    popd
}

case "$1" in
    "mkdir") fMkdir
	     ;;
    "cp:elec") fCpElec
	       ;;
    "styl") fStyl
	    ;;
    "purs") fPurs
	    ;;
    "mods") fMods
	    ;;
    "build") fMkdir
	     fCpElec
	     fStyl
	     fPurs
	     ;;
    "start") npm start
	     ;;
    "dist") fMods
	    fPackageAll
	    ;;
    "dist:lin") fMods
		fPackageLin
		;;
    "zip") fZipAll
	   ;;
    "lin:test") rm -rf temp/lin-x64
		mkdir -p temp
		cp -r "${distOutput}/cdda-modding-helper-facade-linux-x64" temp/lin-x64
		cp *.jar temp/lin-x64
		cp __config.json temp/lin-x64
		;;
esac
