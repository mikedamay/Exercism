module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds / (convert 0.2408467)
ageOn Venus seconds = seconds / (convert 0.61519726)
ageOn Earth seconds = seconds / (convert 1.0)
ageOn Mars seconds = seconds / (convert 1.8808158)
ageOn Jupiter seconds = seconds / (convert 11.862615)
ageOn Saturn seconds = seconds / (convert 29.447498)
ageOn Uranus seconds = seconds / (convert 84.016846)
ageOn Neptune seconds = seconds / (convert 164.79132)

convert :: Float -> Float
convert = (* 31557600)
