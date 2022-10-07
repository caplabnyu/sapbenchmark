import spacy
import pandas as pd
import itertools
import re
import openpyxl
import numpy as np
criticalVERB = "sent"
nlp = spacy.load("en_core_web_sm")
df = pd.read_excel("extracted_"+criticalVERB+".xlsx")
df = pd.DataFrame(df)
withspace = (" "+criticalVERB+" ")
allsents = {}
for i, sent, adjacent in zip(df.index, df.loc[:,'corpus'], df.loc[:,'adjacentwords']):
	k = "sent"+str(i)     #name the key for each sentence in the dictionary
	allsents[k] = {}      #the value of each sentence key itself is another dictionary
	allsents[k]['SENT']=sent   #the first value in the sentence dictionary is the sentence
	if isinstance(sent, str):
		doc = nlp(sent)			#input this sentence into the nlp function
		sts = list()
		for subsentences in doc.sents:
			l = str(subsentences).lower()
			if l.count(withspace)!=0:
				sts.append(subsentences.text)
		allsents[k]['subsentences'] = sts
		allsents[k]['adjacentwords'] = adjacent
	else:
		allsents[k]['subsentences']=[]
		allsents[k]['adjacentwords']=adjacent
numberofverb = []
verb_type = []
MVRR=[]
extract_sentence = []
ambigV = []
wrong_search = []

'''
doc = nlp("The order of expressions along this dimension correlated well with both the median positions in the ranking task ( Spearman 's r = .86 , p < 0.001 ) and median assigned probability ( Spearman 's r = .88 , p < 0.001 ) .")
z = 0
Vposition = 0
directobject = []
criticalVERB = "observed"
for token in doc:
	if token.text == criticalVERB:
		Vposition = z
	if token.head.text==criticalVERB and token.dep_=="dobj":  #if that chunk is a direct object of the target verb
		directobject.append(token.text)
	print("token",token.text)
	print("direct object", directobject)
	print("TAG", token.tag_)
	print("POS", token.pos_)
	print("HEAD", token.head.text)
	print("dep_", token.dep_)
	print("lemma", token.lemma_)
	print("child", [child for child in token.children])
	print("---------")
z = z + 1
'''
for i in allsents:
	if len(allsents[i]['subsentences'])==0:
		numberofverb.append(99)
		verb_type.append("")
		extract_sentence.append("unexplainednoise; manualcheck")
		MVRR.append("")
		ambigV.append("")
		wrong_search.append("")
	elif len(allsents[i]['subsentences'])==1 and allsents[i]['subsentences'][0].count(criticalVERB)==1:
		numberofverb.append(1)
		doc = nlp(allsents[i]['subsentences'][0])
		NotApplicable = 0
		followed_by = ""
		AV=[]   #additional verbs
		AVRoot = 0
		AVassociated = 0  #additional verbs that are associated with the critical verb
		AVnonassociated = 0		#additional verbs that are not associated with the critical verb
		auxiliarated = 0
		ruled_out = 0
		additionalhead=0
		CVwithhead=0
		CVtype = ""
		textinasentence = []
		z = 0
		CVnotthefirstverb = 0
		CVposition = 0
		for chunk in doc:
			if chunk.text!=criticalVERB:
				textinasentence.append(chunk.text)
				z = z+1
			if chunk.text==criticalVERB:
				textinasentence.append(chunk.text)
				CVposition=z
				verb_type.append(chunk.tag_)
				z = z +1
		preceding = textinasentence[0:CVposition]
		following = textinasentence[CVposition+1:len(textinasentence)]
		colon =0
		semico =0 
		dash=[0]
		punct=0
		if ":" in preceding:
			colon = max(np.where(np.array(preceding) == ":")[0])
		if ";" in preceding:
			semico = max(np.where(np.array(preceding) == ";")[0])
		if "--" in textinasentence:
			dash = np.where(np.array(textinasentence) == "--")[0]
		if len(dash)==2:
			if dash[0] < CVposition < dash[1]:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[dash[0]+1:dash[1]])
			elif CVposition < dash[0]:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[0:dash[0]])
			elif CVposition > dash[1]:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[dash[1]+1:len(nlp(allsents[i]['subsentences'][0]))+1])
			doc = nlp(allsents[i]['subsentences'][0])
			z = 0
			CVposition=0
			for chunk in doc:
				if chunk.text==criticalVERB:
					CVposition = z
					verb_type[-1] = chunk.tag_
				z = z+1
		elif len(dash)==1:
			dash=0
			if "--" in preceding:
				dash = preceding.index("--")
			punct = max(colon,semico,dash)
			if punct!=0:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[punct+1:len(nlp(allsents[i]['subsentences'][0]))+1])
				doc = nlp(allsents[i]['subsentences'][0])
				z = 0
				CVposition=0
				for chunk in doc:
					if chunk.text==criticalVERB:
						CVposition = z
						verb_type[-1] = chunk.tag_
					z = z+1
			else:
				colon=0
				semico=0
				dash = 0
				if ":" in following:
					colon = min(np.where(np.array(following) == ":")[0])+1
				if ";" in following:
					semico = min(np.where(np.array(following) == ";")[0])+1
				if "--" in following:
					dash = following.index("--")
				punct = [colon,semico,dash]
				punct = sorted(punct)
				if punct.count(0)==0:
					punct = len(preceding)+min(punct)+1
				elif punct.count(0)==1:
					punct = len(preceding)+punct[1]
				elif punct.count(0)==2:
					punct = len(preceding)+punct[2]
				else:
					punct = 0
				if punct!=0:		
					allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[0:punct+1])
					doc = nlp(allsents[i]['subsentences'][0])
					z = 0
					CVposition=0
					for chunk in doc:
						if chunk.text==criticalVERB:
							CVposition = z
							verb_type[-1] = chunk.tag_
						z = z+1
		z=0
#		if i == "sent568":
#			print(allsents[i]['subsentences'][0])
		for chunk in doc:
			if chunk.text==criticalVERB:
				if chunk.dep_=="ROOT":
					CVtype ="CVasROOT"
				elif chunk.dep_ in ["ccomp", "acl", "advcl"]:
					CVtype ="CVasothers"
				#if chunk.tag_=="VBN":
				#	CVtype ="CVasVBN"
				if doc[z+1].text in ["which","what","where","how","when","who", "whichever","whose", "whoever", "whatever","whenever","wherever","as","up","with","out","in","over","you"]:
					followed_by ="wrongsearch"
			if chunk.text!=criticalVERB:
				if z < CVposition and chunk.pos_ in ["VERB","AUX"] and chunk.tag_!="VBG" and (chunk.tag_!="VBN" and chunk.head.text!=criticalVERB) and (np.sum(np.isin(["while","after","because","before","though","although","as","when","if","once","unless","until","whereas","whenever","whoever","whatever","whether","even"], [kid.lemma_ for kid in chunk.children]))==0) and "to" not in [kid.lemma_ for kid in chunk.children]:
					CVnotthefirstverb += 1
				if z < CVposition and chunk.lemma_ == "have" and ((chunk.head.text==criticalVERB) or (criticalVERB in [kid.text for kid in chunk.children])):
					auxiliarated = auxiliarated +1
				elif chunk.dep_ =="ROOT" and criticalVERB in [child.text for child in chunk.children]:
					AVRoot = AVRoot+1
				elif chunk.tag_ in ["VB","VBD","VBZ","VBP","VBN","MD"] and chunk.head.text==criticalVERB and doc[z-1].tag_!="TO" and (doc[z-1].pos_!="CCONJ" and doc[z-2].pos_!="CCONJ"):
					if chunk.tag_ != "VB" or chunk.tag_ !="VBN":
						AVassociated = AVassociated+1
						AV.append(chunk.text)
					elif chunk.tag_=="VBN":
						if (doc[z-1].pos_=="AUX" or doc[z-2].pos_=="AUX" or doc[z-3].pos_=="AUX"):
							AVassociated = AVassociated+1
							AV.append(chunk.text)
					else:
						aux = 0
						for child in chunk.children:
							if child.dep_=="aux":
								aux = aux+1
							if aux !=0:
								AVassociated = AVassociated+1
								AV.append(chunk.text)
				elif chunk.tag_ == "VBG" and chunk.head.text==criticalVERB and chunk.dep_ in ["ccomp","xcomp"]:
					AVassociated = AVassociated+1
					AV.append(chunk.text)
				elif chunk.pos_ in ["VERB","AUX"] and doc[z-1].tag_!="TO":
					AVnonassociated = AVnonassociated+1
				if z > CVposition and chunk.head.text==criticalVERB and (chunk.dep_=="npadvmod" or ((chunk.text in ["ago", "earlier","later","back"]) and doc[z-1].dep_=="npadvmod")) and (z-CVposition <= 5 ):
					if doc[z-1].dep_!="dobj" and doc[z-2].dep_!="dobj" and doc[z-3].dep_!="dobj" and doc[z-4].dep_!="dobj":
						followed_by="adverbialNP"
			z=z+1
		if (np.sum(np.isin(["while","after","because","before","though","although","as","when","if","once","unless","until","whereas","whenever","whoever","whatever","whether","even","who","which","where","and"], [kid.lemma_ for kid in doc[CVposition].children]))!=0):
			CVnotthefirstverb=0
		z=0
		if CVtype=="CVasROOT":
			for chunk in doc:
				if z < CVposition and chunk.pos_ in ["VERB","AUX"] and chunk.head.text==criticalVERB:
					for child in chunk.children:
						if chunk.dep_ not in ["nsubj","nsubjpass","csubj"] and child.dep_ not in ["nsubj","nsubjpass","csubj"]:   #then it's simply MV
							ruled_out = ruled_out+1
				z=z+1
		z=0
		for chunk in doc:
			if chunk.text!=criticalVERB:
				if chunk.dep_ =="nsubj" and chunk.head.text in AV:
					additionalhead=additionalhead+1
				if chunk.dep_ =="nsubj" and chunk.head.text == criticalVERB:
					CVwithhead = CVwithhead+1
			z=z+1
		extract_sentence.append(allsents[i]['subsentences'][0])
		if auxiliarated!=0:
			MVRR.append(0)
		#elif CVtype=="CVasVBN":
		#	MVRR.append(1)
		elif AVRoot==0 and AVassociated==0 and AVnonassociated==0:
			MVRR.append(2)
		elif ruled_out!=0:
			MVRR.append(3)
		elif (CVtype=="CVasROOT" and additionalhead==0 and len(AV)!=0) or (CVtype=="CVasothers" and (CVwithhead==0 or (additionalhead==0 and len(AV)!=0))): #if only one subject for the ROOTCV or if no nsubj at all:
			MVRR.append(1)
		else:
			MVRR.append(2)   #CV as mainverb or complements but with a head
		if followed_by =="wrongsearch":
			wrong_search.append("wrongsearch")
		elif followed_by =="adverbialNP":
			wrong_search.append("adverbialNP")
		else:
			wrong_search.append("")
		if CVnotthefirstverb==0:
			ambigV.append(1)
		else:
			ambigV.append(0)
#		if i=="sent568":
#			print(AV, AVassociated, AVnonassociated, CVwithhead, auxiliarated, AVRoot, ruled_out, additionalhead, followed_by, CVnotthefirstverb, CVtype)
	elif len(allsents[i]['subsentences'])==1 and (allsents[i]['subsentences']).count(criticalVERB)!=1:
		extract_sentence.append(allsents[i]['subsentences'][0])
		numberofverb.append(99)
		MVRR.append("onesentencemultiplecriticalwords")
		verb_type.append("")
		wrong_search.append("")
		ambigV.append("onesentencemultiplecriticalwords")
	else:
		for p,q in enumerate(allsents[i]['subsentences']):
			if (q.count(allsents[i]['adjacentwords'])==1) and (q.count(criticalVERB)==1):
				numberofverb.append(1)
				doc = nlp(q)
				z = 0
				NotApplicable = 0
				followed_by = ""
				AV=[]
				AVRoot = 0
				AVassociated = 0
				AVnonassociated = 0
				auxiliarated = 0
				ruled_out = 0
				additionalhead=0
				CVwithhead=0
				CVtype = ""
				CVnotthefirstverb = 0
				textinasentence = []
				CVposition = 0
				for chunk in doc:
					if chunk.text!=criticalVERB:
						textinasentence.append(chunk.text)
						z = z+1
					if chunk.text==criticalVERB:
						textinasentence.append(chunk.text)
						CVposition=z
						verb_type.append(chunk.tag_)
						z = z +1
				preceding = textinasentence[0:CVposition]
				following = textinasentence[CVposition+1:len(textinasentence)]
				colon =0
				semico =0
				dash = [0]
				punct=0
				if ":" in preceding:
					colon = max(np.where(np.array(preceding) == ":")[0])
				if ";" in preceding:
					semico = max(np.where(np.array(preceding) == ";")[0])
				if "--" in textinasentence:
					dash = np.where(np.array(textinasentence) == "--")[0]
				if len(dash)==2:
					if dash[0] < CVposition < dash[1]:
						q = str(nlp(q)[dash[0]+1:dash[1]])
					elif CVposition < dash[0]:
						q = str(nlp(q)[0:dash[0]])
					elif CVposition > dash[1]:
						q = str(nlp(q)[dash[1]+1:len(textinasentence)+1])
					doc = nlp(q)
					z = 0
					CVposition=0
					for chunk in doc:
						if chunk.text==criticalVERB:
							CVposition = z
							verb_type[-1] = chunk.tag_
						z = z+1
				elif len(dash)==1:
					dash=0
					if "--" in preceding:
						dash = preceding.index("--")
					punct = max(colon,semico,dash)
					if punct!=0:
						q = str(nlp(q)[punct+1:len(nlp(q))+1])
						doc = nlp(q)
						z = 0
						CVposition=0
						for chunk in doc:
							if chunk.text==criticalVERB:
								CVposition = z
								verb_type[-1] = chunk.tag_
							z = z+1
					else:
						colon=0
						semico=0
						dash=0
						if ":" in following:
							colon = min(np.where(np.array(following) == ":")[0])+1
						if ";" in following:
							semico = min(np.where(np.array(following) == ";")[0])+1
						if "--" in following:
							dash = following.index("--")
						punct = [colon,semico,dash]
						punct = sorted(punct)
						if punct.count(0)==0:
							punct = len(preceding)+min(punct)
						elif punct.count(0)==1:
							punct = len(preceding)+punct[1]
						elif punct.count(0)==2:
							punct = len(preceding)+punct[2]
						else:
							punct = 0
						if punct!=0:		
							q = str(nlp(q)[0:punct+1])
							doc = nlp(q)
							z = 0
							CVposition=0
							for chunk in doc:
								if chunk.text==criticalVERB:
									CVposition = z
									verb_type[-1] = chunk.tag_
								z = z+1
				z=0
#				if i == "sent568":
#					print(allsents[i]['subsentences'][0])
				for chunk in doc:
					if chunk.text==criticalVERB:
						if chunk.dep_=="ROOT":
							CVtype ="CVasROOT"
						elif chunk.dep_ in ["ccomp", "acl", "advcl"]:
							CVtype ="CVasothers"
						#if chunk.tag_=="VBN":
						#	CVtype ="CVasVBN" #override, if VBN
						if doc[z+1].text in ["which","what","where","how","when","who", "whichever","whose", "whoever", "whatever","whenever","wherever","as","up","with","out","in","over","it","you"]:
							followed_by ="wrongsearch"
					if chunk.text!=criticalVERB:
						if z < CVposition and chunk.pos_ in ["VERB","AUX"] and chunk.tag_!="VBG" and (chunk.tag_!="VBN" and chunk.head.text!=criticalVERB) and (np.sum(np.isin(["while","after","because","before","though","although","as","when","if","once","unless","until","whereas","whenever","whoever","whatever","whether","even"], [kid.lemma_ for kid in chunk.children]))==0) and "to" not in [kid.lemma_ for kid in chunk.children]:
							CVnotthefirstverb = 1
						if z < CVposition and chunk.lemma_ =="have" and ((chunk.head.text==criticalVERB) or (criticalVERB in [kid.text for kid in chunk.children])):
							auxiliarated = auxiliarated+1
						elif chunk.dep_ =="ROOT" and criticalVERB in [child.text for child in chunk.children]:
							AVRoot = AVRoot+1
						elif chunk.tag_ in ["VB","VBD","VBZ","VBP","VBN","MD"] and chunk.head.text==criticalVERB and doc[z-1].tag_!="TO" and (doc[z-1].pos_!="CCONJ" and doc[z-2].pos_!="CCONJ"):
							if chunk.tag_ != "VB" or chunk.tag_ !="VBN":
								AVassociated = AVassociated+1
								AV.append(chunk.text)
							elif chunk.tag_ == "VBN":
								if (doc[z-1].pos_=="AUX" or doc[z-2].pos_=="AUX" or doc[z-3].pos_=="AUX"):
									AVassociated = AVassociated+1
									AV.append(chunk.text)
							else:
								aux = 0
								for child in chunk.children:
									if child.dep_=="aux":
										aux = aux+1
									if aux !=0:           #when verb(root-form) and auxiliarated
										AVassociated = AVassociated+1
										AV.append(chunk.text)
						elif chunk.tag_ == "VBG" and chunk.head.text==criticalVERB and chunk.dep_ in ["ccomp","xcomp"]:
							AVassociated = AVassociated+1
							AV.append(chunk.text)
						elif chunk.pos_ in ["VERB","AUX"] and doc[z-1].tag_!="TO":
							AVnonassociated = AVnonassociated+1
						if z > CVposition and chunk.head.text==criticalVERB and (chunk.dep_=="npadvmod" or ((chunk.text in ["ago", "earlier","later","back"]) and doc[z-1].dep_=="npadvmod")) and (z-CVposition <= 5 ):
							if doc[z-1].dep_!="dobj" and doc[z-2].dep_!="dobj" and doc[z-3].dep_!="dobj" and doc[z-4].dep_!="dobj":
								followed_by="adverbialNP"
					z=z+1
				if (np.sum(np.isin(["while","after","because","before","though","although","as","when","if","once","unless","until","whereas","whenever","whoever","whatever","whether","even","who","which","where","and"], [kid.lemma_ for kid in doc[CVposition].children]))!=0):
					CVnotthefirstverb=0
				z=0
				if CVtype=="CVasROOT":
					for chunk in doc:
						if z < CVposition and chunk.pos_ in ["VERB","AUX"] and chunk.head.text==criticalVERB:
							for child in chunk.children:
								if chunk.dep_ not in ["nsubj","nsubjpass","csubj"] and child.dep_ not in ["nsubj","nsubjpass","csubj"]:   #then it's simply MV
									ruled_out = ruled_out+1
						z=z+1
				z=0
				for chunk in doc:
					if chunk.text!=criticalVERB:
						if chunk.dep_ =="nsubj" and chunk.head.text in AV:
							additionalhead=additionalhead+1
						if chunk.dep_ =="nsubj" and chunk.head.text == criticalVERB:
							CVwithhead = CVwithhead+1
					z=z+1
				extract_sentence.append(q)
				if (auxiliarated!=0):
					MVRR.append(0)
				elif AVRoot==0 and AVassociated==0 and AVnonassociated==0:
					MVRR.append(2)
				elif ruled_out!=0:
					MVRR.append(3)
				elif (CVtype=="CVasROOT" and additionalhead==0 and len(AV)!=0) or (CVtype=="CVasothers" and ((CVwithhead==0) or (additionalhead==0 and len(AV)!=0))): #if only one subject for the ROOTCV or if no nsubj at all:
					MVRR.append(1)
				else:
					MVRR.append(2)
				if followed_by =="wrongsearch":
					wrong_search.append("wrongsearch")
				elif followed_by =="adverbialNP":
					wrong_search.append("adverbialNP")
				else:
					wrong_search.append("")
				if CVnotthefirstverb == 0:
					ambigV.append(1)
				else:
					ambigV.append(0)
#				if i=="sent568":
#					print(AV, AVassociated, AVnonassociated, CVwithhead, auxiliarated, AVRoot, ruled_out, additionalhead, followed_by, CVnotthefirstverb, CVtype)
				break
			elif (q.count(allsents[i]['adjacentwords'])==1) and (q.count(criticalVERB)!=1):
				extract_sentence.append(q)
				numberofverb.append(99)
				MVRR.append("onesentencemultiplecriticalwords")
				verb_type.append("")
				wrong_search.append("")
				ambigV.append("onesentencemultiplecriticalwords")
				break
			elif (q.count(allsents[i]['adjacentwords'])>1):
				extract_sentence.append(q)
				numberofverb.append(99)
				MVRR.append("")
				verb_type.append("")
				wrong_search.append("")
				ambigV.append("")
				break
			else:
				if p+1 == len(allsents[i]['subsentences']):
					extract_sentence.append("NA")
					numberofverb.append(99)
					MVRR.append("")
					verb_type.append("")
					wrong_search.append("")
					ambigV.append("")
df['extract_sentence']=extract_sentence
df['numb_of_V']=numberofverb
df['MVRR']=MVRR
df['verb_type']=verb_type
df['wrong_search']=wrong_search
df['ambigV']=ambigV


df.to_excel(r'output/targetsentences_'+criticalVERB+'ambigMVRR.xlsx',header=True)
