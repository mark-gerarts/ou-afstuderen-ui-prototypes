{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
 
main = mainWidget $ el "div" $ text "Welcome to Reflex"
