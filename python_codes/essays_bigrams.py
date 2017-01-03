from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from collections import Counter
import os
import pickle


def get_all_files(directory):
    return os.listdir(directory)


def return_count_dict(arr_of_path):

   if(os.path.isfile("bigram_count_essays.p")):
       print "Pickle for word count found. Returning."
       return pickle.load(open("bigram_count_essays.p", 'rb'))

   c_words = Counter();

   for file in arr_of_path:
        f = open(file,"rb").read().decode("utf-8");
        words = word_tokenize(f);
        pairs = zip(words, words[1:]);
        c_words += Counter(pairs)
        print '1'

   pickle.dump(c_words, open("bigram_count_essays.p", 'wb'))

   return c_words

if __name__ == "__main__":
   all_2014 = get_all_files("../Data/Extracted.Essays/wg14_extracted.essays_matrics/");
   all_2014 = ["../Data/Extracted.Essays/wg14_extracted.essays_matrics/" +x for x in all_2014]
   all_2014 = [x for x in all_2014 if ".txt" in x] #Getting only the txt files

   all_2015 = get_all_files("../Data/Extracted.Essays/wg15_extracted.essays_matrics/");
   all_2015 = ["../Data/Extracted.Essays/wg15_extracted.essays_matrics/" +x for x in all_2015]
   all_2015 = [x for x in all_2015 if ".txt" in x] #Getting only the txt files

   all_2016 = get_all_files("../Data/Extracted.Essays/wg16_extracted.essays_matrics/");
   all_2016 = ["../Data/Extracted.Essays/wg16_extracted.essays_matrics/" +x for x in all_2016]
   all_2016 = [x for x in all_2016 if ".txt" in x]  #Getting only the txt files


   all_files = all_2014 + all_2015 + all_2016
   print len(all_files)

   all_count_dict = return_count_dict(all_files)
   all_count_dict_copy = all_count_dict

   to_rem = [];
   for key in all_count_dict:
       if("." in key[0] or "," in key[0] or "WG" in key[0] or "-" in key[0] or "." in key[1] or "," in key[1] or "WG" in key[1] or "-" in key[1] or "(" in key[0] or "(" in key[1] or ")" in key[0] or  ")" in key[1] or ":" in key[0] or ":" in key[1] or "'" in key[0] or "'" in key[1]):
           to_rem.append(key)

   for items in to_rem:
       all_count_dict.pop(items)
   # all_count_dict = all_count_dict_copy
   # all_count_dict.pop(',')
   # all_count_dict.pop('.') #Removing , and .
   # all_count_dict.pop('_14_Essays')
   # all_count_dict.pop('WG')
   # all_count_dict.pop('!')
   # all_count_dict.pop(':')
   # all_count_dict.pop('?')
   # all_count_dict.pop(';')
   # all_count_dict.pop("''")
   # all_count_dict.pop("(")
   # all_count_dict.pop(")")
   # all_count_dict.pop("``")
   # all_count_dict.pop(u'-')
   # all_count_dict.pop(u'\u2013')
   # all_count_dict.pop(u'\u201d')
   # all_count_dict.pop('--')


   print all_count_dict.most_common(2000)



   # print all_count_dict.most_common(2000)

   f = open("top2k_bigram_list.txt","w")
   for a in all_count_dict.most_common(2000):
       key = a[0]
       f.write(key[0].encode("utf-8")+" " +key[1].encode("utf-8"))
       f.write("\n")
