> import FRP.Reactive


> -- Convert minutes to seconds
> mToS :: Double -> Double
> mToS = (*) 60
> -- The appropriate time to cook a soft-boiled egg is 3 minutes (see wikipedia
> eggTimer :: Event ()
> eggTimer = atTime (mToS 3)

