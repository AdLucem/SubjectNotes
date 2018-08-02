module Lib where

-- neural network type
data NeuralNetwork = NeuralNetwork {
                                    input :: Float,
                                    weights :: [[Float]],
                                    biases :: [Float]
                                   } deriving (Show, Read)

-- some math stuff
e :: Float
e = 2.718281828459045

sigmoid :: Float -> Float
sigmoid x = e**x / (e**x + 1.0)
 
layer :: Float -> [Float] -> Float -> Float
layer input weights bias = 
    sigmoid $ (foldl (+) 0 [ input*w | w <- weights]) + bias 

feedForward :: NeuralNetwork -> Float
feedForward nn = 
    let
        i = input nn
        wt_1 = (weights nn) !! 0
        wt_2 = (weights nn) !! 1
        b_1 = (biases nn) !! 0
        b_2 = (biases nn) !! 1
    in
        let
            i_2 = layer i wt_1 b_1
        in
            layer i_2 wt_2 b_2

     

