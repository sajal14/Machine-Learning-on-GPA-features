from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from collections import Counter
import os
import pickle
import re

def get_all_files(directory):
    return os.listdir(directory)



def genX(appIds_list,bigrams_list,all_matrics):

    appId_wordcnt = {}

    if(os.path.isfile("appId_bigramcnt.p")):
        print "Pickle for appId_wordcnt found. Returning."
        appId_wordcnt =  pickle.load(open("appId_bigramcnt.p", 'rb'))
    else:
        for file in all_matrics:
            f= open(file,"r").read().decode("utf-8")
            appId_arr = re.findall(r'\b(\d{7})\b',f)
            appId = ""
            print 1
            if(appId_arr):
                appId = appId_arr[0]
                if(appId in appIds_list):
                    words = word_tokenize(f)
                    pairs = zip(words, words[1:]);
                    appId_wordcnt[appId] = Counter(pairs)
            else:
                print file

        pickle.dump(appId_wordcnt, open("appId_bigramcnt.p", 'wb'))

    #All appid and their word count stored in a dict#

    # print appId_wordcnt

    out_X = open("bigram_feature.csv","w")
    out_X.write("AppIds,")
    out_X.write(",".join(bigrams_list))
    out_X.write("\n")


    for appId in appIds_list:
        print appId
        out_X.write(appId)
        out_X.write(",")
        if appId not in appId_wordcnt:
            temp = ["-1"]*2000
            out_X.write(",".join(temp))
        else:
            app_counter = appId_wordcnt[appId]
            for word in bigrams_list:
                tup = tuple(word.split(" "))
                out_X.write(str(app_counter[tup]))
                out_X.write(",")
        out_X.write("\n")




def get_essay_wordcnt(file_path, available_appids):#Counts word count of all the essays and validate it against the app

    if(os.path.isfile("appId_wordcnt.p")):
       print "Pickle for appId_wordcnt found. Returning."
       return pickle.load(open("appId_wordcnt.p", 'rb'))


    appId_wordcnt = {}
    for file in file_path:
        f= open(file,"r").read().decode("utf-8")
        appId_arr = re.findall(r'\b(\d{7})\b',f)
        appId = ""
        if(appId_arr):
            appId = appId_arr[0]

        # appIdOrder = f.open("appId_order.txt","w")


        if appId in available_appids: #appId present in the csv
            words = word_tokenize(f);
            appId_wordcnt[appId] = Counter(words)
        print "n"

    pickle.dump(appId_wordcnt, open("appId_wordcnt.p", 'wb'))
    return appId_wordcnt


def generateX_with_appID(appId_wordcnt,appId_val_dict,vocab_path): #Write
    vocab = open(vocab_path,"r").read().split("\n")
    vocab_list = [x.split(" ")[1] for x in vocab]
    vocab_list = vocab_list

    GPA_appIds = open("gpa_appIds.txt","w")
    GPA_X = open("gpa_X.txt","w")

    for appId in appId_wordcnt.keys():
        for entry in vocab_list:
            c = appId_wordcnt[appId]
            if(entry.decode("utf-8") in c.keys()):
                GPA_X.write(str(c[entry])+",")
            else:
                GPA_X.write(str(0)+",")
        GPA_X.write("\n")
        GPA_appIds.write(str(appId)+"\n")




def generateY_using_appIdfile(appId_filePath,appId_val_dict):
    f = open(appId_filePath,"r").read()
    lines = f.split("\n")
    GPA_labels = open("gpa_labels.txt","w")
    for entry in lines:
        if(entry in appId_val_dict.keys()):
            GPA_labels.write(appId_val_dict[entry][0])
        else:
            GPA_labels.write("UNK")
        GPA_labels.write("\n")


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


    # f = open("Data/entries.csv").read()
    # f_line = f.strip().split("\n")
    #
    # appId_val_dict = {} #Containing keys as appId, and value as [GPA, rating1, rating2] for all items present in the csv
    # for line in f_line:
    #     f_arr = line.split(",")
    #     appId_val_dict[f_arr[1]] = [ f_arr[2] , f_arr[3] , f_arr[4]]

    bigrams_list = open("top2k_bigram_list.txt","r").read().split("\n")

    appIds_list = open("../arranged_data/ayids_order.txt").read().split("\n")

    genX(appIds_list,bigrams_list,all_matrics)

    #
    # appId_wordcnt = get_essay_wordcnt(all_matrics,appId_val_dict.keys()) #Dict with key = appId for ONLY THOSE WHICH ARE PRESENT IN BOTH TEXT AND CSV, value = Counter of words.
    #
    # # generateX_with_appID(appId_wordcnt,appId_val_dict,"vocab_list.txt")
    #
    # generateY_using_appIdfile("input_gpa/gpa_appIds.txt",appId_val_dict)