#!/bin/sh

HOSTNAME="anryoshi-mac"

scutil --set HostName "$HOSTNAME"
scutil --set LocalHostName "$HOSTNAME"
scutil --set ComputerName "$HOSTNAME"

# Dock
defaults write com.apple.dock "tilesize" -int "36" 
defaults write com.apple.dock "show-recents" -bool "false"
killall Dock

# Finder
defaults write NSGlobalDomain "AppleShowAllExtensions" -bool "true"
defaults write com.apple.finder "AppleShowAllFiles" -bool "true"
defaults write com.apple.finder "ShowPathbar" -bool "true"
defaults write com.apple.finder "_FXSortFoldersFirst" -bool "true"
defaults write com.apple.finder "FXDefaultSearchScope" -string "SCcf"
defaults write com.apple.finder "FXEnableExtensionChangeWarning" -bool "false"
defaults write NSGlobalDomain "NSDocumentSaveNewDocumentsToCloud" -bool "false"
defaults write com.apple.finder "ShowMountedServersOnDesktop" -bool "true"
killall Finder

# TextEdit
defaults write com.apple.TextEdit "RichText" -bool "false"
killall TextEdit

# Time Machine
defaults write com.apple.TimeMachine "DoNotOfferNewDisksForBackup" -bool "true" 
