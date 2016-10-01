# Interference Computing

Not much here yet, just a start to modeling the interference logic gate and a visualization tool, Scratch.

OutputListener: To read output from the machine, we need to take in an arbitrary function, f(t), and say whether a frequency is heard in a time interval. 

Since we have working radio communications, this is a solved problem, but I'm lazy and don't want anything here complicated enough to break yet. So, I've dealt with it by sampling f(t) over the time interval and throwing out any points less than a threshold (threshold > |f(t)|). Hopefully, all the noise falls beneath the threshold, leaving us with the peaks and valleys. In each peak and valley, assume the curve is symmetric and average the time to get an estimate of f(t)'s maximas and minimas. The distance between a maxima and the next minima is half a wave length, which gives us the frequency. 

It's a two parameter model, where you have to specify a noise threshold and number of points to sample. Sampling is done randomly, instead of at regular intervals, because regular intervals would have beat frequencies with f(t) -- which would only cause confusing shit to happen.

