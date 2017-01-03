from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from collections import Counter
import os
import pickle

def get_all_files(directory):
    return os.listdir(directory)

def flatten(listoflists):
        flat_list = []
        for sublist in listoflists:
            for items in sublist:
                flat_list.append(items)
        return flat_list


def return_count_dict(arr_of_path):

   if(os.path.isfile("word_count_all_essays.p")):
       print "Pickle for word count found. Returning."
       return pickle.load(open("word_count_all_essays.p", 'rb'))

   c_words = Counter();

   for file in arr_of_path:
        f = open(file,"rb").read().decode("utf-8");
        words = word_tokenize(f);
        c_words += Counter(words)


   pickle.dump(all_count_dict, open("word_count_all_essays.p", 'wb'))

   return c_words


def reset_stopword_cnt(count_dict):
    stop = stopwords.words("english")
    stop.append(".")
    stop.append(",")
    stop.append("The")
    stop.append("WG")
    stop.append("_14_Essays")
    stop.append("(")
    stop.append(")")
    for a in stop:
        if a in count_dict.keys():
            count_dict[a] = 0

    return count_dict



if __name__ == "__main__":
   all_2014 = get_all_files("Data/Extracted.Essays/wg14_extracted.essays_all/");
   all_2014 = ["Data/Extracted.Essays/wg14_extracted.essays_all/" +x for x in all_2014]
   all_2014 = [x for x in all_2014 if ".txt" in x] #Getting only the txt files

   all_2015 = get_all_files("Data/Extracted.Essays/wg15_extracted.essays_all/");
   all_2015 = ["Data/Extracted.Essays/wg15_extracted.essays_all/" +x for x in all_2015]
   all_2015 = [x for x in all_2015 if ".txt" in x] #Getting only the txt files

   all_2016 = get_all_files("Data/Extracted.Essays/wg16_extracted.essays_all/");
   all_2016 = ["Data/Extracted.Essays/wg16_extracted.essays_all/" +x for x in all_2016]
   all_2016 = [x for x in all_2016 if ".txt" in x]  #Getting only the txt files


   all_files = all_2014 + all_2015 + all_2016
   all_count_dict = return_count_dict(all_files)

   print len(all_count_dict.keys())

   dict_no_stopwrds = reset_stopword_cnt(all_count_dict)

   print len(all_count_dict.keys())

   print dict_no_stopwrds.most_common(40)

   #Write top 5000 words in file
   # f = open("vocab_list.txt","w")
   # top_5k = dict_no_stopwrds.most_common(5000)
   # i = 0;
   # for tup in top_5k:
   #     f.write(str(i) +" " + tup[0].encode("utf-8")+"\n")
   #     i = i+1




