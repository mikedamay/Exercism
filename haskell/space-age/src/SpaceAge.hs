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
ageOn planet seconds = convert planet seconds

convert planet = seconds / (yearOn planet * 31557600)

yearOn Mercury = 0.2408467
yearOn Venus = 0.61519726
yearOn Earth = 1.0
yearOn Mars = 1.8808158
yearOn Jupiter = 11.862615
yearOn Saturn = 29.447498
yearOn Uranus = 84.016846
yearOn Neptune = 164.79132



