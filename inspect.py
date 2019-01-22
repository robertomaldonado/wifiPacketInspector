import copy

samples = [] #Structure to hold the values of input
K = 7  #Constant defined for Viterbi
STATES = []
idx = 0

# Provided code to extract the packet size from the decoded output
def get_length(states):
    PKT_LEN = 0
    for j in range(1,12):
        PKT_LEN = PKT_LEN + states[17-j]
        PKT_LEN = PKT_LEN * 2
    return PKT_LEN

def main():
    global STATES, samples, idx
    LIMIT = 2**(K-1)

    for num in range( LIMIT ): #64         
	l = list( format(num,'06b') ) #Format
	STATES.append( list ( [l,9999] ) ) #Make list and add weight
    STATES[0][1] = 0 #set weight of first state to 0

    #Load the file into the program
    f = open("LTSOutput.txt", "r")
    for line in f:
    	samples.extend([int(i) for i in line.split(" ")]) #Extend and split on space found

    for h in range( len(samples)/2 ):
        statesCopy = copy.deepcopy( STATES ) #Copy by value
        for m in range( LIMIT/2 ): #Iterate half to then change vals
            for i in range( 2 ): #Iterate twice over reg, with 0 and 1

                sliceObj = slice(h,h+6) #Set the sliced object

                B = statesCopy[m][0][sliceObj]
                B.reverse()
                out_a = i ^ int(B[1]) ^ int(B[2]) ^ int(B[4]) ^ int(B[5])
                #out_a = (i + int(B[5]) + int(B[1]) + int(B[4]) + int(B[2]))%2
                #out_b = (i + int(B[5]) + int(B[0]) + int(B[2]) + int(B[1]))%2
                out_b = i ^ int(B[0]) ^ int(B[1]) ^ int(B[2]) ^ int(B[5])
                opt1 = [out_a, out_b]

                B = statesCopy[m+32][0][sliceObj]
                B.reverse()
                out_a = i ^ int(B[1]) ^ int(B[2]) ^ int(B[4]) ^ int(B[5])
                #out_a = (i + int(B[5]) + int(B[1]) + int(B[4]) + int(B[2]))%2
                #out_b = (i + int(B[5]) + int(B[0]) + int(B[2]) + int(B[1]))%2
                out_b = i ^ int(B[0]) ^ int(B[1]) ^ int(B[2]) ^ int(B[5])
                opt2 = [out_a, out_b]        
                
                EOUT = [samples[2*h], samples[2*h+1]]

                costopt1 = 0
                for x in range( len(opt1) ):
                    if(opt1[x]!=EOUT[x]):
                        costopt1 = costopt1 + 1
                costopt2 = 0
                for x in range( len(opt2) ):
                    if(opt2[x]!=EOUT[x]):
                        costopt2 = costopt2 + 1

                if( (costopt1 + statesCopy[m][1]) < (costopt2 + statesCopy[m+32][1]) ):
                    STATES[m*2+i][0] = list(statesCopy[m][0])
                    STATES[m*2+i][0].append(i)
                    STATES[m*2+i][1] = statesCopy[m][1] + costopt1
                else:
                    STATES[m*2+i][0] = list(statesCopy[m+32][0])
                    STATES[m*2+i][0].append(i)
                    STATES[m*2+i][1] = statesCopy[m+32][1] + costopt2

    #Erase 6 trailing zeros in all final chains
    for j in range( len(STATES) ):
        for i in range(6):
            del STATES[j][0][0]
    
    min = 9999
    for i in range(LIMIT):
        if min < STATES[i][1]:
            idx = i
            min = STATES[i][1]

main() #Execute main function
print "Length found in packet: %s" % get_length( (STATES[idx]) [0] ) #Print the result
#for i in range(64):
#    print "%s" % STATES[i][1]