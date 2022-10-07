import spacy
import pandas as pd
import itertools
import re
import openpyxl
import numpy as np
criticalVERB = "showed"
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
numberofdobj = []
dobj = []
verb_type = []
extract_sentence = []
'''
doc = nlp("Visitors to the country observed prisoners being marched in leg irons , metal collars , or")
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
		numberofdobj.append("")
		dobj.append("")
		verb_type.append("")
		extract_sentence.append("unexplainednoise; manualcheck")
	elif len(allsents[i]['subsentences'])==1 and allsents[i]['subsentences'][0].count(criticalVERB)==1:
		numberofverb.append(1)
		doc = nlp(allsents[i]['subsentences'][0])
		#initialization
		directobject = []
		NotApplicable = 0
		criticalV_toV_Ving = 0
		w = 0
		z = 0  #iterator for words in the critical sentence
		textinasentence=[]
		CVposition = 0
		#index critical word's posiiton in the critical sentence
		for chunk in doc:
			if chunk.text!=criticalVERB:
				textinasentence.append(chunk.text)
				z = z+1
			if chunk.text==criticalVERB:
				textinasentence.append(chunk.text)
				CVposition=z
				z = z +1
		preceding = textinasentence[0:CVposition]
		following = textinasentence[CVposition+1:len(textinasentence)]
		#identify punctuations that are not periods
		colon =0
		semico =0
		dash=[0]
		punct=0
		if ":" in preceding:
			colon = max(np.where(np.array(preceding) == ":")[0])
		if ";" in preceding:
			semico = max(np.where(np.array(preceding) == ";")[0])
		if "--" in textinasentence:
			dash = np.where(np.array(textinasentence)== "--")[0]
		if len(dash)==2:
			if dash[0] < CVposition < dash[1]:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[dash[0]+1:dash[1]+1])
			elif CVposition < dash[0]:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[0:dash[0]+1])
			elif CVposition > dash[1]:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[dash[1]+1:len(nlp(allsents[i]['subsentences'][0]))+1])
			doc = nlp(allsents[i]['subsentences'][0])
		elif len(dash)==1:
			dash=0
			if "--" in preceding:
				dash = preceding.index("--")
			punct = max(colon,semico,dash)
			if punct!=0:
				allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[punct+1:len(nlp(allsents[i]['subsentences'][0]))+1])
				doc = nlp(allsents[i]['subsentences'][0])
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
					punct = len(preceding)+min(punct)+1
				elif punct.count(0)==1:
					punct = len(preceding)+punct[1]
				elif punct.count(0)==2:
					punct = len(preceding)+punct[2]
				else:
					punct=0
				if punct!=0:		
					allsents[i]['subsentences'][0] = str(nlp(allsents[i]['subsentences'][0])[0:punct+1])
					doc = nlp(allsents[i]['subsentences'][0])
		z = 0
		Vposition = 0
		Verbencountered = []
		for chunk in doc:
			if (chunk.pos_ in ["VERB","AUX"]) or (chunk.text==criticalVERB):
				Verbencountered.append(chunk.text)
			if chunk.text == criticalVERB:
				verb_type.append(chunk.tag_)
				Vposition = z
				if doc[z+1].text in ["which","what","where","how","when","who", "whichever","whose", "whoever", "whatever","whenever","wherever","as","up","with","out","in","over"]:
					w = 1            #but mark those sentences with wh- immediately following the target verb (for later rejection)
			if chunk.head.text==criticalVERB and chunk.dep_=="dobj":  #if that chunk is a direct object of the target verb
				directobject.append(chunk.text)  #then keep this chunk as part of a list [direct object]
			if Vposition!=0 and chunk.head.text==criticalVERB and (chunk.dep_=="npadvmod" or ((chunk.text in ["ago", "earlier","later","back"]) and doc[z-1].dep_=="npadvmod")) and (z-Vposition <= 5 ):
				if doc[z-1].dep_!="dobj" and doc[z-2].dep_!="dobj" and doc[z-3].dep_!="dobj" and doc[z-4].dep_!="dobj":
					NotApplicable = NotApplicable + 1
			if Vposition!=0 and chunk.head.text==criticalVERB and z>Vposition and (chunk.pos_=="VERB" or chunk.pos_=="AUX") and (doc[z-1].tag_=="TO" or doc[z-2].tag_=="TO") and len(directobject)==0:
				criticalV_toV_Ving = criticalV_toV_Ving + 1              #claimed NOUN to V
			if Vposition!=0 and chunk.head.text==criticalVERB and z>Vposition and chunk.tag_ in ["VBG","VBN"] and len(directobject)==0 and re.search(" be$",doc[Vposition:z].lemma_)==None and re.search(" be ",doc[Vposition:z].lemma_)==None and re.search(",",doc[Vposition:z].text)==None  and re.search(" have$",doc[Vposition:z].lemma_)==None and re.search(" have ",doc[Vposition:z].lemma_)==None and re.search(" 'd$",doc[Vposition:z].lemma_)==None and re.search(" 'd ",doc[Vposition:z].lemma_)==None and re.search(" 's$",doc[Vposition:z].lemma_)==None and re.search(" 's ",doc[Vposition:z].lemma_)==None:
				criticalV_toV_Ving = criticalV_toV_Ving + 1               #remembered NOUN Ving
			z=z+1
		allsents[i]['Verbencountered']=Verbencountered
		allsents[i]['directobject']=directobject
		extract_sentence.append(allsents[i]['subsentences'][0])
		if len(allsents[i]['directobject'])==0 and w==0 and criticalV_toV_Ving==0 and NotApplicable==0: #no direct object and no toV and Ving and no Wh-
			numberofdobj.append(0)
			dobj.append(list(allsents[i]['directobject']))  #which is null []
		elif w==1: #the complement starts with Wh-
			numberofdobj.append("NA")
			dobj.append(["wrongsearch"])
		elif len(allsents[i]['directobject'])==0 and criticalV_toV_Ving!=0:
			numberofdobj.append(1)
			dobj.append(["toVorVing"])  #in case of claimed sth to V or remembered sth Ving
		elif len(allsents[i]['directobject'])==0 and NotApplicable !=0:
			numberofdobj.append("NA")
			dobj.append(["AdverbialNoun"])
		elif len(allsents[i]['directobject'])==1:
			numberofdobj.append(1)
			dobj.append(list(allsents[i]['directobject']))
		else:
			numberofdobj.append(99)
			dobj.append(list(allsents[i]['directobject']))
	elif (len(allsents[i]['subsentences'])==1) and (str(allsents[i]['subsentences']).count(criticalVERB)!=1):
		extract_sentence.append(allsents[i]['subsentences'][0])
		numberofverb.append(99)
		numberofdobj.append("")
		dobj.append(None)
		verb_type.append(None)
	else:
		for p, q in enumerate(allsents[i]['subsentences']):
			if (q.count(allsents[i]['adjacentwords'])==1) and (q.count(criticalVERB)==1):
				numberofverb.append(1)
				doc = nlp(q)
				directobject = []
				NotApplicable = 0
				w = 0
				z = 0  #iterator for words in the critical sentence
				criticalV_toV_Ving = 0
				textinasentence=[]
				CVposition = 0
				for chunk in doc:
					if chunk.text!=criticalVERB:
						textinasentence.append(chunk.text)
						z = z+1
					if chunk.text==criticalVERB:
						textinasentence.append(chunk.text)
						CVposition=z
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
						q = str(nlp(q)[dash[0]+1:dash[1]+1])
					elif CVposition < dash[0]:
						q = str(nlp(q)[0:dash[0]+1])
					elif CVposition > dash[1]:
						q = str(nlp(q)[dash[1]+1:len(nlp(q))+1])
					doc = nlp(q)
				elif len(dash)==1:
					dash = 0
					if "--" in preceding:
						dash = preceding.index("--")
					punct = max(colon,semico,dash)
					if punct!=0:
						q = str(nlp(q)[punct+1:len(nlp(q))+1])
						doc = nlp(q)
					else:
						colon=0
						semico=0
						dash=0
						if ":" in following:
							colon = len(preceding)+min(np.where(np.array(following) == ":")[0])+1
						if ";" in following:
							semico = len(preceding)+min(np.where(np.array(following) == ";")[0])+1
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
				z=0
				Vposition = 0
				Verbencountered=[]
				for chunk in doc:
					if (chunk.pos_ in ["VERB", "AUX"]) or (chunk.text==criticalVERB):
						Verbencountered.append(chunk.text)
					if chunk.text == criticalVERB:
						verb_type.append(chunk.tag_)
						Vposition = z
						if doc[z+1].text in ["which","what","where","how","when","who", "whichever","whose", "whoever", "whatever","whenever","wherever","as","up","with","out","in","over"]:
							w = 1            #but mark those sentences with wh- immediately following the target verb (for later rejection)
					if chunk.head.text==criticalVERB and chunk.dep_=="dobj":  #if that chunk is a direct object of the target verb
						directobject.append(chunk.text)  #then keep this chunk as part of a list [direct object]
					if Vposition!=0 and chunk.head.text==criticalVERB and (chunk.dep_=="npadvmod" or ((chunk.text in ["ago", "earlier","later","back"]) and doc[z-1].dep_=="npadvmod")) and (z-Vposition <= 5 ):
						if doc[z-1].dep_!="dobj" and doc[z-2].dep_!="dobj" and doc[z-3].dep_!="dobj" and doc[z-4].dep_!="dobj":
							NotApplicable = NotApplicable + 1
					if Vposition!=0 and chunk.head.text==criticalVERB and z>Vposition and (chunk.pos_=="VERB" or chunk.pos_=="AUX") and (doc[z-1].tag_=="TO" or doc[z-2].tag_=="TO") and len(directobject)==0:
						criticalV_toV_Ving = criticalV_toV_Ving + 1              #claimed NOUN to V
					if Vposition!=0 and chunk.head.text==criticalVERB and z>Vposition and chunk.tag_ in ["VBG","VBN"] and len(directobject)==0 and re.search(" be$",doc[Vposition:z].lemma_)==None and re.search(" be ",doc[Vposition:z].lemma_)==None and re.search(",",doc[Vposition:z].text)==None  and re.search(" have$",doc[Vposition:z].lemma_)==None and re.search(" have ",doc[Vposition:z].lemma_)==None and re.search(" 'd$",doc[Vposition:z].lemma_)==None and re.search(" 'd ",doc[Vposition:z].lemma_)==None and re.search(" 's$",doc[Vposition:z].lemma_)==None and re.search(" 's ",doc[Vposition:z].lemma_)==None:
						criticalV_toV_Ving = criticalV_toV_Ving + 1               #remembered NOUN Ving
					z=z+1
				allsents[i]['Verbencountered']=Verbencountered
				allsents[i]['directobject']=directobject
				extract_sentence.append(q)
				if len(allsents[i]['directobject'])==0 and w==0 and criticalV_toV_Ving==0 and NotApplicable==0: #no direct object and no toV and Ving and no Wh-
					numberofdobj.append(0)
					dobj.append(list(allsents[i]['directobject']))  #which is null []
					break
				elif w==1: #the complement starts with Wh-
					numberofdobj.append("NA")
					dobj.append(["wrongsearch"])
					break
				elif len(allsents[i]['directobject'])==0 and criticalV_toV_Ving!=0:
					numberofdobj.append(1)
					dobj.append(["toVorVing"])  #in case of claimed sth to V or remembered sth Ving
					break
				elif len(allsents[i]['directobject'])==0 and NotApplicable !=0:
					numberofdobj.append("NA")
					dobj.append(["AdverbialNoun"])
					break
				elif len(allsents[i]['directobject'])==1:
					numberofdobj.append(1)
					dobj.append(list(allsents[i]['directobject']))
					break
				else:
					numberofdobj.append(99)
					dobj.append(list(allsents[i]['directobject']))
					break
			elif (q.count(allsents[i]['adjacentwords'])==1) and (q.count(criticalVERB)!=1):
				extract_sentence.append(q)
				numberofverb.append("onesentencemultiplecriticalwords")
				numberofdobj.append(99)
				dobj.append(None)
				verb_type.append(None)
				break
			elif (q.count(allsents[i]['adjacentwords'])>1):
				extract_sentence.append(q)
				numberofverb.append("onesentencemultiplecriticalwords")
				numberofdobj.append("")
				dobj.append("")
				verb_type.append("")
				break
			else:
				if p+1 == len(allsents[i]['subsentences']):
					extract_sentence.append("NA")
					numberofverb.append(99)
					numberofdobj.append("")
					dobj.append("")
					verb_type.append("")
#	print(i, len(verb_type))

df['extract_sentence'] = extract_sentence
df['numb_of_CV']=numberofverb
df['numb_of_dobj']=numberofdobj
df['dobj']=dobj
df['verb_type']=verb_type


for sent in allsents:
	text = list()            #placeholder for each word
	POS = list()            #placeholder for the POS of each word
	tg = list()				#placeholder for the tag (precise POS) of each word
	if len(allsents[sent]['subsentences'])==1 and allsents[sent]['subsentences'][0].count(criticalVERB)==1:
		doc = nlp(allsents[sent]['subsentences'][0])			#input this sentence into the nlp function
		for token in doc:		#iterate all the words in the sentence and get their text, pos and tag
			text.append(token.text)
			POS.append(token.pos_)
			tg.append(token.tag_)
		allsents[sent]['text']=text
		allsents[sent]['PoS']=POS
		allsents[sent]['TAG']=tg
	elif len(allsents[sent]['subsentences'])>1:
		for p, q in enumerate(allsents[sent]['subsentences']):
			if (q.count(allsents[sent]['adjacentwords'])==1) and (q.count(criticalVERB)==1):
				doc=nlp(q)
				for token in doc:
					text.append(token.text)
					POS.append(token.pos_)
					tg.append(token.tag_)
				allsents[sent]['text']=text
				allsents[sent]['PoS']=POS
				allsents[sent]['TAG']=tg
				break
			else:
				if p == len(allsents[sent]['subsentences'])-1:
					allsents[sent]['text']=[]
					allsents[sent]['PoS']=[]
					allsents[sent]['TAG']=[]
	else:
		allsents[sent]['text']=[]
		allsents[sent]['PoS']=[]
		allsents[sent]['TAG']=[]
###applying heuristics
for i in allsents:
	k = allsents[i]['text']
	if(len(k)!=0):
		lk = [items.lower() for items in k]
		Vposition = lk.index(criticalVERB)
		endofsentence = [':',".",";","#","!"]
		terminal = list()
		for signal in endofsentence:
			if signal in k and k.index(signal)>Vposition:
				terminal.append(k.index(signal))
			else:
				terminal.append(len(k))
		stop = min(terminal)   #the earliest of any of them is the termial point
		allsents[i]['criticalWds'] = allsents[i]['TAG'][Vposition+1:stop]   #critical words are all words after the critical verb and before the terminal point
	else:
		allsents[i]['criticalWds'] = []
for i in allsents:  #if there's a verb or aux in the ending regions"
	n = 0
	for v in ["VBD","VBZ","VBP","MD","VB"]:
		if v in allsents[i]['criticalWds']:
			n = n+1
		else:
			n = n
	if n!= 0 :
		allsents[i]['followed_by_mainverb'] = 1
	else:
		allsents[i]['followed_by_mainverb'] = 0
	z = 0
	m = 0
	s = 0
	lk = [items.lower() for items in allsents[i]['text']]
	if len(lk)!=0:
		Vposition = lk.index(criticalVERB)
	for token in allsents[i]['text']:
		z = z +1
		if len(allsents[i]['text'])!=0 and z >=2:
			if token.lower()=="that" and allsents[i]['text'][z-2]==criticalVERB:
				m = m+1
			else:
				m = m
		if len(allsents[i]['text'])!=0:
				if z-1 < Vposition and token in ["While","After","Because","Before","Though","Although","As","When","If","Once","Unless","Until","Whereas","Whenever","Whoever","Whatever","Whether","Even"]:
					inbetween = allsents[i]['text'][z-1:Vposition]
					if inbetween.count(",")==0:
					# While ...., criticalV ...
						s = s+1
					else:
						s = s
	if len(allsents[i])==10:
		if len(allsents[i]['Verbencountered'])!=0 and allsents[i]['Verbencountered'][0]!=criticalVERB:
			#more than one verb in the same clause but the first verb is not the critical verb
			s =0
	if m!=0:
		allsents[i]['followed_by_that'] = 1
	else:
		allsents[i]['followed_by_that'] = 0
	if s!=0:
		allsents[i]['subordinated'] = 1
	else:
		allsents[i]['subordinated'] = 0
newcolumn = []
for i in allsents:
	newcolumn.append(allsents[i]['followed_by_mainverb'])
newcolumn2 = []
for i in allsents:
	newcolumn2.append(allsents[i]['followed_by_that'])
newcolumn3 = []
for i in allsents:
	newcolumn3.append(allsents[i]['subordinated'])

df['followed_by_mainverb']=newcolumn
df['followed_by_that']=newcolumn2
df['subordinated']=newcolumn3
df.to_excel(r'output/targetsentences_'+criticalVERB+'.xlsx',header=True)