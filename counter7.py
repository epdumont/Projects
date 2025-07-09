#Homework 7
#Emily Dumont
#
#Problem 7: Find and return the most frequent words
#Convert the dictionary to a list of tuples in order to return the ten most frequent words

import string

class Count:
        def __init__(self):
                infile = open("stop_words.txt", "r")
                self.stop_words = []
                for stop in infile:
                    stop = stop.strip()
                    self.stop_words.append(stop)
                self.word_counts = {}
                print("Word Counter Initialized")

                infile.close()

        def get_num_words(self):
                return len(self.word_counts)

        def increment_count(self,word):
                word = word.lower()
                word = word.strip(string.punctuation)
                if word == "":
                    return
                if word in self.stop_words:
                    return
                if word in self.word_counts:
                        self.word_counts[word] += 1
                else:
                        self.word_counts[word] = 1
                              
        def lookup_count(self,word):
                if word in self.word_counts:
                        return self.word_counts[word]
                else:
                        return 0
                
        def get_top_words(self,num):
                lst = [] #create an empty list
                for key,value in self.word_counts.items(): #write a loop to produce a list of tuples. ".items" makes key and value each their own item as separated by the comma
                        tupl = (value,key) #swap the items so that they are sorted by the count
                        lst.append(tupl) #append the tuple of each top ten word from above into the empty list
                lst.sort(reverse = True) #sort the list by the value(aka the word count) and reverse that so that the keys are in descending order
                #append a tuple through a loop
                return lst[:num] #return the list starting from the beginning of the most frequent values until the top ten have been returned
         
def main():
        counter = Count()
        
        filename = input("Enter book file:")
        infile = open(filename)

        for line in infile:
                line = line.strip()
                word = line.split()
                for i in word:
                        counter.increment_count(i)
                
        infile.close()
                    
        print("Top 10 Words:")
        top_ten = counter.get_top_words(10)
        print(top_ten)

        print("Unique words:", counter.get_num_words())
        
main()
        
