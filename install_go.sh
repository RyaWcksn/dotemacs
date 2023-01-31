#!/bin/bash

function gopher {
cyan=`tput setaf 14` 
beige=`tput setaf 215` 
white=`tput setaf 15` 
black=`tput setaf 0` 
red=`tput setaf 9` 
reset=`tput sgr0` 
teeth="${white}UU" 
if [[ "$1" == "vampire" ]]
then
	teeth="${red}VV" 
fi
echo "${cyan}      ´.-::::::-.´"
echo "${cyan}  .:-::::::::::::::-:."
echo "${cyan}  ´_::${white}:    ::    :${cyan}::_´"
echo "${cyan}   .:${white}( O   :: O   )${cyan}:."
echo "${cyan}   ´::${white}:   ${beige}(${black}..${beige})${white}   :${cyan}::."
echo "${cyan}   ´:::::::${teeth}${cyan}:::::::´"
echo "${cyan}   .::::::::::::::::."
echo "${beige}   O${cyan}::::::::::::::::${beige}O"
echo "${cyan}   -::::::::::::::::-"
echo "${cyan}   ´::::::::::::::::´"
echo "${cyan}    .::::::::::::::."
echo "${beige}      oO${cyan}:::::::${beige}Oo"
echo "${reset}"
}


architecture ()
{
    architecture=""
    case $(uname -m) in
        i386)   architecture="386" ;;
        i686)   architecture="386" ;;
        x86_64) architecture="amd64" ;;
        arm)    dpkg --print-architecture | grep -q "arm64" && architecture="arm64" || architecture="arm" ;;
        *) echo "Unknown architecture" break
    esac
    echo $architecture
}

prepare_package ()
{
    packagesNeeded='wget'
    if [ -x "$(command -v apk)" ];       then sudo apk add --no-cache $packagesNeeded
    elif [ -x "$(command -v apt-get)" ]; then sudo apt-get install $packagesNeeded
    elif [ -x "$(command -v dnf)" ];     then sudo dnf install $packagesNeeded
    elif [ -x "$(command -v zypper)" ];  then sudo zypper install $packagesNeeded
    elif [ -x "$(command -v brew)" ];  then brew install $packagesNeeded
    elif [ -x "$(command -v pacman)" ];  then sudo pacman -S $packagesNeeded
    else echo "FAILED TO INSTALL PACKAGE: Package manager not found. You must manually install: $packagesNeeded">&2; fi
}

download_source_code ()
{
    version=$1
    architecture=$(architecture)
    os=$(uname | awk '{print tolower($0)}')
    if [[ $(which wget) == "" ]]; then
        echo "Wget not found..."
        echo "Downloading..."
        prepare_package
        continue
    fi
    echo "    Downloading GO..."
    echo "    OS := ${os} "
    echo "    Architecture := ${architecture} "
    echo "    Version := ${version} "
    gopher
    mkdir -p "Golang"
    cd "Golang/"
    if [[ $(ls | grep ${version}) == "" ]]; then
        wget https://go.dev/dl/go$version.$os-$architecture.tar.gz 
        tar -xvf go$version.$os-$architecture.tar.gz 
        continue
    fi
    if [[ $(go version) != "go version go${version} ${os}/${architecture}" ]]; then
        if [[ $os == "linux" ]]; then
	    echo "Installing for Linux..."
            sudo cp -r go /usr/bin/
	    echo "Installation for Linux is done!"
        elif [[ $os == "darwin" ]]; then
	    echo "Installing for MacOs..."
            sudo cp -r go /usr/local/
	    echo "installation for MacOs is done!"
        fi
        continue
    fi

    echo "GO Version ${version} is installed"
}

main ()
{
    while true;do
        case "$1" in
        -v)
            download_source_code $2
            break
        ;;
        *)
            echo "Script usage: $0 [-v version]"
            break
        ;;
        esac 
    done
}

main $1 $2
