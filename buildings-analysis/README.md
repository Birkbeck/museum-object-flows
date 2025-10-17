# Building Analysis

An automated pipeline for modelling notes regarding museum buildings

## The pipeline

### Preprocess buildings data

Buildings notes are augmented with collection/reasons notes where reference is made to them.

### Train classifier

10% of notes are manually labelled with labels classifying buildings' change in use, status, and responsibility.

The classifier is trained with a support set of 1% of labels and an evaluation set of 9%.

### Classifier analysis

The prompt and hyperparameters which best approximates the evaluation set (measured by cosine similarity of label embeddings) are selected.

### Run classifier

The rest of the documents are labelled automatically with the LLM+prompt+hyperparameters which performed best on the evaluation set.

### Labels to taxonomy

Label embedding and k-means are used to find a small number of clusters of similar labels. A generative LLM is used to produce a label which describes each cluster.

This process is repeated on each cluster to produce a hierarchy with two layers.