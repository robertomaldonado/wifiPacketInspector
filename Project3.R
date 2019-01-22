###############################################
#    Section 0: Load data to program    # 
###############################################
noise_signal = 500 #Noise guarenteed to be fisrt 500 samples
#Read data into program
read_data = read.csv(file = "r/pkt5", sep=" ")
#Get Signal and change it to magnitude
z = complex(real = read_data[,1], imaginary = read_data[,2])

magn = Re(Conj(z) * z)
#Get the STS and change it to magnitude
t_sts = read.csv(file = "r/sts", head = TRUE, sep=" ")
sts = complex(real = t_sts[,1], imaginary = t_sts[,2])
sts_magn = Re( sts * Conj(sts) )
#Plot the magnitudes of STS
plot(sts_magn, type = "l")
#Plot the magnitudes of Signal
plot(magn, type = "l")

#First Part. Smooth and locate high power signal
#Smooth
smooth_magn = magn
for(x in 1:length(magn)-1){
  smooth_magn[x] = magn[x]*0.9 + magn[x+1]*0.1
}

#############################################
#  Section 1: Get initial index to look for Short Training Simbol # 
#############################################  
#Find the average power sent by noise
cur_sum = 0
for(x in 1:noise_signal){
  cur_sum = cur_sum + smooth_magn[x]
}

#Scan for the high power signal
flag_value = 0
for(x in 1:length(smooth_magn)){
  if( smooth_magn[x] > 4*(cur_sum/noise_signal)){
    flag_value = x
    break
  }
}

flag_value #Laoded where the change of power is evident
#############################################
#  Section 2: Scan 10 signals for the STS   # 
#############################################
#Cross-correlation between sample and signal at position
ini = flag_value - 2
fin = ini + 32

ans = ccf(sts_magn, magn[ini:fin])
max_acf = which.max( ans$acf )
#print(sprintf("Corr:%s", ans))

correction = 0
correction = correction - ans$lag[max_acf]

sym_ini = ini + correction
sym_fin = ini + correction + 32

print(sprintf("Ending value of symbol in index: %s", sym_fin-1))

for (i in 1:9){
  ans = ccf(magn[sym_ini:sym_fin],sts_magn)
  id = which.max( ans$acf )
  
  correction = 0
  correction = correction - ans$lag[id]
  sym_ini = sym_ini + correction
  sym_fin = sym_fin + correction + 32
  # View(correction)
  #print(sprintf("Corr:%s", ans))
  #print(sprintf("Ending value of symbol in index: %s", sym_fin-1))
}
print(sprintf("Finished with the STSs scan at index position: %s", sym_fin-1))

# #############################################
# #  PART  II: LEARN CHANNEL FACTOR           # 
# #############################################
# 10385
# startbit = 11308;
startbit = sym_fin;
firstLTS = startbit + 64;
secondLTS = firstLTS + 128;

# plot(Re(offset_est), type = "l" , xlim=c(0, 320), tck=-0.01)

begin = firstLTS + ( 7 * (128/8) )
end = secondLTS + (7*(128/8)-1)
lts = z[begin:end]

#View plot
#plot(Re(lts), type = "l" , xlim=c(0, 128), tck=-0.01)

reduced_lts = matrix(0, ncol = 1 , nrow = 64)
#Taking only 64 samples
temp = 1
#Long Training Symbol
for (i in 1:128){
  if (!i %% 2){ #If even skip
    next
  }
  reduced_lts[temp] = lts[i];
  temp = temp + 1
}
#Debug lts
#length(reduced_lts)
#reduced_lts
#plot(reduced_lts, type = "l", ylim=c(0, 6e+07) , tck=-0.01)
#End Debug

#Load OFDM sequence
OFDM_Signal = c(-1, 1, -1, -1, 1, 1, -1, 1, -1, 1, -1, -1, -1, -1, -1, 1, 1, -1, -1, 1, -1, 1, -1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, -1, 1, -1, 1, 1, 1, 1, 1, 1, -1, -1, 1, 1, -1, 1, -1, 1, 1, 1, 1);

#Get the normalized fft (Fast Fourier transform)
channel_fft = fft(reduced_lts, inverse = FALSE)
#Manually normalized fft
# d = matrix(0, ncol = 1 , nrow = 64)
# 
# n = length(reduced_lts)
# n
# reduced_lts
# for (p in 1:n){
#   sum=0
#   for (q in 1:n){
#     exponential = exp( -2*pi*1i*(q-1)*(p-1)/n )
#     sum  = sum + reduced_lts[q]*exponential
#   }
#   d[p] = sum;
# }
# d

#y[h] = sum_{k=1}^n z[k]*exp(-2*pi*1i*(k-1)*(h-1)/n
#channelFactor = OFDM_Signal * t(d)
channelFactor  = OFDM_Signal * channel_fft
#Get the channel factor by doing the fft times OFDM_signal
#channelFactor = t(channel_fft) * OFDM_Signal
#Print the channel factor
#channelFactor

#############################################
#  PART  III: GET THE PLCP                  # 
#############################################

PLCPstart = secondLTS + 128
PLCPsignal = PLCPstart + 16
plcp_end = PLCPsignal+127

temporal_plcp = z[c(PLCPsignal:plcp_end)];

#Taking only 64 samples
reduced_plcp = matrix(0, ncol = 1 , nrow = 64)

temp = 1
for (i in 1:128){
  if (!i %% 2){ #If even skip
    next
  }
  reduced_plcp[temp] = temporal_plcp[i];
  temp = temp + 1
}
#Debug reduced_plcp
#length(reduced_plcp)
#plot(Re(reduced_plcp), type = "l" , tck=-0.01)
#Print reduced_plcp
#reduced_plcp
#FFT for the reduced_plcp
red_plcp = fft(reduced_plcp, inverse = FALSE)

# result = matrix(0, ncol = 1 , nrow = 64)
# n = length(reduced_plcp)
# for (p in 1:n){
#   sum=0
#   for (q in 1:n){
#     exponential = exp( -2*pi*1i*(q-1)*(p-1)/n )
#     sum  = sum + reduced_plcp[q]*exponential
#   }
#   result[p] = sum;
# }
# result

# red_plcp = t(result)
#Normalize data
normalizedData = ( red_plcp / channelFactor )
#Print the obtained fft
#red_plcp
#channelFactor
#normalizedData

#length(normalizedData)
#Remove carrier not needed in signal
normalizedData = normalizedData[-c(1,8,22,28,29,30,31,32,33,34,35,36,37,38,44,58)]
#length(normalizedData)

hard_quan_PLCP_48 = matrix(0, ncol = 48 , nrow = 1)

#Swap halves
for(i in 25:48){
  if(Re(normalizedData[i]) > 0 ){
    hard_quan_PLCP_48[i-24]= 1
  }else{
    hard_quan_PLCP_48[i-24] = -1
  }
}
for(i in 1:24){
  if(Re(normalizedData[i]) > 0 ){
    hard_quan_PLCP_48[i+24] = 1 
  }else{
    hard_quan_PLCP_48[i+24] = -1
  }
}
#Print the result as vector
#as.vector(hard_quan_PLCP_48)

#De-interleave 
#Vector to hold final 
hard_quan_PLCP_data = matrix(0, ncol = 48 , nrow = 1)
#Do de-interleave
hard_quan_PLCP_data [(1+0)] = hard_quan_PLCP_48[(1+0)]
hard_quan_PLCP_data [(1+1)] = hard_quan_PLCP_48 [(1+3)] 
hard_quan_PLCP_data [(1+2)] = hard_quan_PLCP_48 [(1+6)] 
hard_quan_PLCP_data [(1+3)] = hard_quan_PLCP_48 [(1+9)] 
hard_quan_PLCP_data [(1+4)] = hard_quan_PLCP_48 [(1+12)] 
hard_quan_PLCP_data [(1+5)] = hard_quan_PLCP_48 [(1+15)] 
hard_quan_PLCP_data [(1+6)] = hard_quan_PLCP_48 [(1+18)] 
hard_quan_PLCP_data [(1+7)] = hard_quan_PLCP_48 [(1+21)] 
hard_quan_PLCP_data [(1+8)] = hard_quan_PLCP_48 [(1+24)] 
hard_quan_PLCP_data [(1+9)] = hard_quan_PLCP_48 [(1+27)] 
hard_quan_PLCP_data [(1+10)] = hard_quan_PLCP_48 [(1+30)] 
hard_quan_PLCP_data [(1+11)] = hard_quan_PLCP_48 [(1+33)] 
hard_quan_PLCP_data [(1+12)] = hard_quan_PLCP_48 [(1+36)] 
hard_quan_PLCP_data [(1+13)] = hard_quan_PLCP_48 [(1+39)] 
hard_quan_PLCP_data [(1+14)] = hard_quan_PLCP_48 [(1+42)] 
hard_quan_PLCP_data [(1+15)] = hard_quan_PLCP_48 [(1+45)] 
hard_quan_PLCP_data [(1+16)] = hard_quan_PLCP_48 [(1+1)] 
hard_quan_PLCP_data [(1+17)] = hard_quan_PLCP_48 [(1+4)] 
hard_quan_PLCP_data [(1+18)] = hard_quan_PLCP_48 [(1+7)] 
hard_quan_PLCP_data [(1+19)] = hard_quan_PLCP_48 [(1+10)] 
hard_quan_PLCP_data [(1+20)] = hard_quan_PLCP_48 [(1+13)] 
hard_quan_PLCP_data [(1+21)] = hard_quan_PLCP_48 [(1+16)] 
hard_quan_PLCP_data [(1+22)] = hard_quan_PLCP_48 [(1+19)] 
hard_quan_PLCP_data [(1+23)] = hard_quan_PLCP_48 [(1+22)] 
hard_quan_PLCP_data [(1+24)] = hard_quan_PLCP_48 [(1+25)] 
hard_quan_PLCP_data [(1+25)] = hard_quan_PLCP_48 [(1+28)] 
hard_quan_PLCP_data [(1+26)] = hard_quan_PLCP_48 [(1+31)] 
hard_quan_PLCP_data [(1+27)] = hard_quan_PLCP_48 [(1+34)] 
hard_quan_PLCP_data [(1+28)] = hard_quan_PLCP_48 [(1+37)] 
hard_quan_PLCP_data [(1+29)] = hard_quan_PLCP_48 [(1+40)] 
hard_quan_PLCP_data [(1+30)] = hard_quan_PLCP_48 [(1+43)]  
hard_quan_PLCP_data [(1+31)] = hard_quan_PLCP_48 [(1+46)] 
hard_quan_PLCP_data [(1+32)] = hard_quan_PLCP_48 [(1+2)] 
hard_quan_PLCP_data [(1+33)] = hard_quan_PLCP_48 [(1+5)] 
hard_quan_PLCP_data [(1+34)] = hard_quan_PLCP_48 [(1+8)] 
hard_quan_PLCP_data [(1+35)] = hard_quan_PLCP_48 [(1+11)] 
hard_quan_PLCP_data [(1+36)] = hard_quan_PLCP_48 [(1+14)] 
hard_quan_PLCP_data [(1+37)] = hard_quan_PLCP_48 [(1+17)] 
hard_quan_PLCP_data [(1+38)] = hard_quan_PLCP_48 [(1+20)] 
hard_quan_PLCP_data [(1+39)] = hard_quan_PLCP_48 [(1+23)] 
hard_quan_PLCP_data [(1+40)] = hard_quan_PLCP_48 [(1+26)] 
hard_quan_PLCP_data [(1+41)] = hard_quan_PLCP_48 [(1+29)] 
hard_quan_PLCP_data [(1+42)] = hard_quan_PLCP_48 [(1+32)] 
hard_quan_PLCP_data [(1+43)] = hard_quan_PLCP_48 [(1+35)] 
hard_quan_PLCP_data [(1+44)] = hard_quan_PLCP_48 [(1+38)] 
hard_quan_PLCP_data [(1+45)] = hard_quan_PLCP_48 [(1+41)] 
hard_quan_PLCP_data [(1+46)] = hard_quan_PLCP_48 [(1+44)] 
hard_quan_PLCP_data [(1+47)] = hard_quan_PLCP_48 [(1+47)] 
#Print the final data
hard_quan_PLCP_data

write.table(hard_quan_PLCP_data, file = "LTSOutput.txt")