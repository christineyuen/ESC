import os
import numpy as np
import pandas as pd
import sklearn.preprocessing
#from utils.utils import create_directory
#import sys
#from utils.utils import generate_results_csv
#import utils


def read_dataset(input_dir):
    return tuple(pd.read_csv(os.path.join(input_dir, file_name), header=None, index_col= None).values.copy() for file_name in ("x_sim.csv", "y_sim.csv", "x.csv", "y.csv"))


def create_directory(directory_path):
    if os.path.exists(directory_path):
        return None
    else:
        try:
            os.makedirs(directory_path)
        except:
            # in case another machine created the path meanwhile !:(
            return None
        return directory_path

def fit_classifier(dataset, classifier_name, output_directory):
    x_train, y_train, x_test, y_test = dataset

    nb_classes = len(np.unique(np.concatenate((y_train, y_test), axis=0)))

    # transform the labels from integers to one hot vectors
    enc = sklearn.preprocessing.OneHotEncoder(categories='auto')
    enc.fit(np.concatenate((y_train, y_test), axis=0).reshape(-1, 1))
    y_train = enc.transform(y_train.reshape(-1, 1)).toarray()
    y_test = enc.transform(y_test.reshape(-1, 1)).toarray()

    # save orignal y because later we will use binary
    y_true = np.argmax(y_test, axis=1)

    if len(x_train.shape) == 2:  # if univariate
        # add a dimension to make it multivariate with one dimension
        x_train = x_train.reshape((x_train.shape[0], x_train.shape[1], 1))
        x_test = x_test.reshape((x_test.shape[0], x_test.shape[1], 1))

    input_shape = x_train.shape[1:]
    classifier = create_classifier(classifier_name, input_shape, nb_classes, output_directory)

    classifier.fit(x_train, y_train, x_test, y_test, y_true)


def create_classifier(classifier_name, input_shape, nb_classes, output_directory, verbose=False):
    if classifier_name == 'fcn':
        from classifiers import fcn
        return fcn.Classifier_FCN(output_directory, input_shape, nb_classes, verbose)
    if classifier_name == 'mlp':
        from classifiers import mlp
        return mlp.Classifier_MLP(output_directory, input_shape, nb_classes, verbose)
    if classifier_name == 'resnet':
        from classifiers import resnet
        return resnet.Classifier_RESNET(output_directory, input_shape, nb_classes, verbose)
    if classifier_name == 'mcnn':
        from classifiers import mcnn
        return mcnn.Classifier_MCNN(output_directory, verbose)
    if classifier_name == 'tlenet':
        from classifiers import tlenet
        return tlenet.Classifier_TLENET(output_directory, verbose)
    if classifier_name == 'twiesn':
        from classifiers import twiesn
        return twiesn.Classifier_TWIESN(output_directory, verbose)
    if classifier_name == 'encoder':
        from classifiers import encoder
        return encoder.Classifier_ENCODER(output_directory, input_shape, nb_classes, verbose)
    if classifier_name == 'mcdcnn':
        from classifiers import mcdcnn
        return mcdcnn.Classifier_MCDCNN(output_directory, input_shape, nb_classes, verbose)
    if classifier_name == 'cnn':  # Time-CNN
        from classifiers import cnn
        return cnn.Classifier_CNN(output_directory, input_shape, nb_classes, verbose)
    if classifier_name == 'inception':
        from classifiers import inception
        return inception.Classifier_INCEPTION(output_directory, input_shape, nb_classes, verbose)


############################################### main

# input
#root_dir = '/Users/loktingyuen/Library/Mobile Documents/com~apple~CloudDocs/PhD/ResNet-ESC'
root_dir = '/Users/loktingyuen/Documents/sim_result/ts_classification_through_simulation/NN/512'
classifier_name = 'resnet'
data_sub_dir = 'sim_data'
result_sub_dir = 'results'

for i in range(1, 101):
    for j in range(1, 23):
        iter_i = str(i)
        dataset_name = str(j)

        # output_dir must end with "/"
        output_dir = os.path.join(root_dir, result_sub_dir, dataset_name, iter_i)+"/"
        input_dir = os.path.join(root_dir, data_sub_dir, dataset_name, data_sub_dir, iter_i)
        create_directory(output_dir)

        dataset = read_dataset(input_dir)

        fit_classifier(dataset, classifier_name, output_dir)

        print('DONE')

        create_directory(output_dir + '/DONE')
