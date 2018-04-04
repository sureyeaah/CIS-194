{-# LANGUAGE RecursiveDo #-}

module Main where
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

main :: IO ()
main = start hello

hello :: IO ()
hello = do
    f     <- frame      [text := "WX Demo"]
    money <- textEntry f [text := "5"]

    add   <- button f   [text := "Insert coin"]
    buy   <- button f   [text := "Buy banana"]

    quit  <- button f   [text := "Quit", on command := close f]
   
    price <- hslider f True 1 20 [selection := 10] 
    priceText <- textEntry f [text:= "10"]

    set f [layout :=
        margin 5 $ column 5 $ map hfill
            [ widget add
            , widget buy
            , widget money
            , widget priceText
            , widget price
            , widget quit
            ]]
    
    let coinMachine :: MomentIO ()
        coinMachine = mdo
            eCoin <- event0 add command
            eBuyPress <- event0 buy command

            let bCanBuy     = (>=) <$> bMoney <*> bPrice
                eBananaSold = whenE bCanBuy eBuyPress

            {-bPrice <- accumB 10 $ (\x -> min 20 $ x+1) <$ eBananaSold-}

            bMoney <- accumB 5 $ unions
                [ (+1) <$ eCoin
                , subtract <$> bPrice <@ eBananaSold
                ]

            let showDialog :: IO ()
                showDialog = infoDialog f "Yummy" "You bought a banana."

                eventSlider :: Slider a -> MomentIO (Event Int)
                eventSlider w = do
                    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 command)
                    fromAddHandler $ mapIO (const $ get w selection) addHandler

            ePriceChange <- eventSlider price
            bPrice <- stepper 10 ePriceChange

            sink buy [ enabled :== bCanBuy ]
            sink money [ text :== show <$> bMoney ]
            sink priceText [ text :== show <$> bPrice ]
            reactimate (showDialog <$ eBananaSold)

    eventNetwork <- compile coinMachine
    actuate eventNetwork
