from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from collections import Counter
import os
import pickle
import re

def get_all_files(directory):
    return os.listdir(directory)


def genfeatures(appIds_list,all_matrics):

    appId_wordcnt = {}

    if(os.path.isfile("appId_wordcnt.p")):
        print "Pickle for appId_wordcnt found. Returning."
        appId_wordcnt =  pickle.load(open("appId_wordcnt.p", 'rb'))
    else:
        return


    out_X = open("essay_features.csv","w")
    out_X.write("AppIds,")
    out_X.write(",".join(["vocab_rich","avg_word_len","total_words"]))
    out_X.write("\n")


    for appId in appIds_list:
        out_X.write(str(appId)+ ",")
        print appId
        if appId not in appId_wordcnt:
            out_X.write(",".join(["-1","-1","-1"]))
            out_X.write("\n")
        else:
            words_dict = appId_wordcnt[appId];
            vocab_rich = len(words_dict.keys())
            tot_length = sum([len(x) for x in words_dict.keys()])
            avg = tot_length/vocab_rich
            count_words = sum(words_dict.values())

            out_X.write(str(vocab_rich)+ ",")
            out_X.write(str(avg)+ ",")
            out_X.write(str(count_words))
            out_X.write("\n")




if __name__ == "__main__":
    matrics_2014 = get_all_files("../Data/Extracted.Essays/wg14_extracted.essays_matrics/");
    matrics_2014 = ["../Data/Extracted.Essays/wg14_extracted.essays_matrics/" +x for x in matrics_2014]
    matrics_2014 = [x for x in matrics_2014 if ".txt" in x] #Getting only the txt files

    matrics_2015 = get_all_files("../Data/Extracted.Essays/wg15_extracted.essays_matrics/");
    matrics_2015 = ["../Data/Extracted.Essays/wg15_extracted.essays_matrics/" +x for x in matrics_2015]
    matrics_2015 = [x for x in matrics_2015 if ".txt" in x] #Getting only the txt files

    matrics_2016 = get_all_files("../Data/Extracted.Essays/wg16_extracted.essays_matrics/");
    matrics_2016 = ["../Data/Extracted.Essays/wg16_extracted.essays_matrics/" +x for x in matrics_2016]
    matrics_2016 = [x for x in matrics_2016 if ".txt" in x] #Getting only the txt files

    all_matrics = matrics_2014 + matrics_2015 + matrics_2016



    appIds_list = open("../arranged_data/ayids_order.txt").read().split("\n")

    # genX(appIds_list,vocab_list,all_matrics)
    genfeatures(appIds_list,all_matrics)