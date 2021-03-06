{ config, pkgs, ... }:

{
imports =
  [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./cachix.nix
  ];

# Use the systemd-boot EFI boot loader.
boot.loader.systemd-boot.enable = true;
boot.loader.efi.canTouchEfiVariables = true;

nixpkgs.config.allowUnfree = true;
boot.initrd.kernelModules = [ "wl" ];
boot.kernelModules = [ "kvm-intel" "wl" ];
boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
boot.kernelPatches = [
  { name = "poweroff-fix"; patch = /etc/poweroff-fix.patch; }
];

boot.extraModprobeConfig = ''
  options libata.force=noncq
  options snd_hda_intel index=0 model=intel-mac-auto id=PCH
  options snd_hda_intel index=1 model=intel-mac-auto id=HDMI
  options snd_hda_intel model=mbp101
  options hid_apple fnmode=2
'';

networking.hostName = "guardian"; 
# networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
networking.networkmanager.enable = true;
      
system.autoUpgrade.enable = true;
system.autoUpgrade.allowReboot = false;

virtualisation.docker.enable = true;

networking.useDHCP = false;
networking.interfaces.wlp3s0.useDHCP = true;

time.timeZone = "Europe/Istanbul";

nix.binaryCaches = [
  http://cache.nixos.org
];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
environment.systemPackages = with pkgs; [
	vim
	traceroute
	go-mtpfs
	htop
	wget
	curl
	dpkg
	git
	zsh
	tmux
	termite
	taffybar
	dmenu
	pciutils
	cachix
	gcc
	rofi
	rofi-calc
	rofi-pass
	docker
	docker-compose
	xorg.xbacklight
];

sound.enable = true;
hardware.pulseaudio.enable = true;
hardware.bluetooth.enable = false;
hardware.facetimehd.enable = true;

services.xserver.enable = true;
services.xserver.videoDrivers = [ "nvidia" ];
services.xserver.dpi = 137;
services.xserver.autorun = true;
services.xserver.layout = "us";
services.xserver.libinput.enable = true;
services.xserver.xkbVariant = "colemak";
services.xserver.windowManager.xmonad = {
	enable = true;
	enableContribAndExtras = true;
	extraPackages = haskellPackages: [
		haskellPackages.xmonad-contrib
		haskellPackages.xmonad-extras
		haskellPackages.xmonad
	];
};
services.xserver.displayManager.defaultSession = "none+xmonad";

services.upower.enable = true;
systemd.services.upower.enable = true;
services.mbpfan = {
    enable = true;
    lowTemp = 61;
    highTemp = 64;
    maxTemp = 84;
  };

users.users.canavar = {
  isNormalUser = true;
  shell = pkgs.zsh;
  home = "/home/canavar";
  extraGroups = [ "wheel" "networkmanager" "docker" ];
};

environment.variables = { EDITOR = "vim"; };

users.users.canavar.packages = with pkgs; [
	neovim
	python38Packages.pynvim
	brave
	discord
	axel
	tdesktop
	qbittorrent
	ghc	
	stack
	qutebrowser
	go
	tdesktop
	teams
	flutter
	subdl
	python38Packages.subliminal
	subberthehut
	mpv
	vlc
	bitwarden
	bitwarden-cli
	python38
	oh-my-zsh
	tree
	file
	bandwhich
	wpsoffice
	evince
	firefox
	opera
	inkscape
	xclip
	polybar
	lm_sensors
	xbindkeys
	xorg.xmodmap
	killall
	taskwarrior
	siji
	tamsyn
	material-icons
	nerdfonts
	fira-code-symbols
	gucharmap
	gnome3.gnome-characters
	ripgrep
	thefuck
	starship
	kubectl
	exa
	autojump
	fzf
	fzf-zsh
	zsh-powerlevel10k
	tmuxPlugins.yank
	tmuxPlugins.sensible
	tmuxPlugins.battery
	nmap
	ncat
	vimPlugins.spacevim
	rustup
	cargo
	rustracer
	go-langserver
	(import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc865
	unzip
	simplescreenrecorder
	flameshot
	networkmanager_dmenu
];


fonts.fonts = with pkgs; [
  corefonts
  dejavu_fonts
  font-awesome-ttf
  inconsolata
  liberation_ttf
  terminus_font
  ubuntu_font_family
  unifont
  noto-fonts
  noto-fonts-emoji
  noto-fonts-extra
  font-awesome
  roboto
  roboto-mono
];

programs.zsh.ohMyZsh = {
  enable = true;
  plugins = [ "git" "python" "man" ];
  theme = "agnoster";
};

programs.zsh = {
  enable = true;
  shellAliases = {
    vim = "nvim";
  };
  enableCompletion = true;
  autosuggestions.enable = true;
  interactiveShellInit = ''
    # z - jump around
    source ${pkgs.fetchurl {url = "https://github.com/rupa/z/raw/2ebe419ae18316c5597dd5fb84b5d8595ff1dde9/z.sh"; sha256 = "0ywpgk3ksjq7g30bqbhl9znz3jh6jfg8lxnbdbaiipzgsy41vi10";}}
    export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh
    export ZSH_THEME="norm"
    export EDITOR='nvim'
    source $ZSH/oh-my-zsh.sh
  '';
  promptInit = "";
};

system.stateVersion = "20.09"; # Did you read the comment?

nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

}

