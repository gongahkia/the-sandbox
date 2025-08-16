# !NOTE
    # gpt4.o implementation of nlp powered entity recognition, enabling text processing and sentence parsing
    # nlp is the baby brother of langchain-powered LLMs

# FUA 
    # run the sanitising functions first before extracting speakers
    # change this so it reads the body paragraph instead but cleaned up
    # just fhand.open() or something
    # write a permutation() function that generates all possible kinds of strings that can be generated for a given set of text inputs, such as a mix of lower and uppercase etc.
    # find other available models and methods I can use to tweak the existing data and extract valid fields with NLP integration
    # update the README.md for the elegant-elefant-internship extraction

import spacy

nlp = spacy.load('en_core_web_sm')

# FUA this array needs to be dynamically generated
text_array = [
    "Dr. Intan Azura Mokhtar asked the Minister for Education about the SkillsFuture initiative. Mr. Ong Ye Kung responded by detailing employment outcomes for graduates.",
    "\n     \n       EDUCATION OF HANDICAPPED CHILDREN  \n      \n     \n       \n          2.   \n         \n           Dr Tan Cheng Bock   \n           asked the First Deputy Prime Minister and Minister of Education why his Ministry is not willing to shoulder the responsibility of educating our handicapped children like the deaf, the blind and the retarded and whether such a move will deprive them of the best education possible.   \n      \n     \n       \n         \n           The Minister of State of Education (Dr Tay Eng Soon) (for the First Deputy Prime Minister and Minister of Education):   \n           Mr Speaker, Sir, I am replying on behalf of my Minister.   \n      \n     \n        Far from being unwilling to shoulder the responsibility, the Government has and will continue to give substantial assistance to voluntary organizations in the education of handicapped children. The Government's policy is to encourage the efforts of the voluntary organizations which are running the special schools and to support them wherever possible. The Ministry of Education has been providing teachers to teach in the eight special schools. Similarly, the Ministry of Social Affairs has been providing instructors for the School for the Blind and the School for the Deaf. In addition, the Ministries of Social Affairs and Health give annual grants to the special schools to meet part of their recurrent expenditure.   \n      \n     \n        In 1982, 69 government teachers and 11 instructors were sent to the special schools. The Ministry of Education had also agreed to deploy another 25 teachers to the special schools last year. The total government subsidy per pupil in the special schools taking into account the grants provided, the teachers and instructors provided was $1,626 per pupil in 1982. This is 41% more than the subsidy of $1,153 per pupil in our regular primary schools in 1982.   \n      \n     \n        In terms of physical facilities, two school buildings have been made available to the special schools at nominal rental. The Ministry is also assisting the Singapore Association for Retarded Children to take over the Griffiths School building for the Association's use on a nominal rental basis."
]

def collect_sentences(doc):
    document_sentences_array = []
    for sent in doc.sents:
        # print(sent.text)
        document_sentences_array.append(sent.text.strip())
    return document_sentences_array

def collect_tokens(doc):
    token_map = {}
    for token in doc:
        token_map[token.idx] = token.text
    return token_map

def collect_dependencies(doc):
    dependancy_map = {}
    for token in doc:
        dependancy_map[token.text] = [token.dep, token.head.text]
    return dependancy_map

def collect_entities(doc):
    entities_map = {}
    for ent in doc.ents:
        entities_map[ent.label_] = ent.text
    return entities_map

def collect_lemmas(doc):
    lemma_array = {}
    for token in doc:
        lemma_array[token.text] = token.lemma_
    return lemma_array

def identify_speakers(doc):
    speakers = []
    for ent in doc.ents:
        if ent.label_ == "PERSON":
            speakers.append(ent.text.strip())
    return speakers

def identify_noun_chunks(doc):
    noun_chunks = []
    for chunk in doc.noun_chunks:
        noun_chunks.append(chunk.text)
    return noun_chunks

# ----- execution code -----

for text in text_array:
    doc = nlp(text)
    print(f"speakers identified: {identify_speakers(doc)}")