# 22 Aug 2022
# Script to classify spectrograms using PNW-Cnet v4. Can also be run without 
# arguments in R using reticulate::source_python(), allowing the makePredictions
# function to be used directly in R.

import os, sys, time
import pandas as pd
import tensorflow as tf

# Returns a Pandas DataFrame, which R can handle more or less natively. Faster
# than writing the predictions to a CSV file and then reading it into R.
# DataFrame has one column Filename for image filenames (not full paths) and 51
# columns AEAC:ZEMA for the class scores.
def makePredictions(target_dir, model_path, show_prog = False):
    class_names = ['AEAC', 'BRCA', 'BRMA', 'BUVI', 'CAGU', 'CALU', 'CAUS', 'CCOO', 
                    'CHFA', 'CHMI', 'CHMI_IRREG', 'COAU', 'COAU2', 'COCO', 'CYST', 
                    'DEFU', 'DOG', 'DRPU', 'DRUM', 'FLY', 'FROG', 'GLGN', 'HOSA', 
                    'HYPI', 'INSP', 'IXNA', 'MEKE', 'MYTO', 'NUCO', 'OCPR', 'ORPI', 
                    'PAFA', 'PECA', 'PHNU', 'PIMA', 'POEC', 'PSFL', 'SHOT', 'SITT', 
                    'SPRU', 'STOC', 'STOC_IRREG', 'STVA', 'STVA_IRREG', 'TADO1', 
                    'TADO2', 'TAMI', 'TUMI', 'WHIS', 'YARD', 'ZEMA']

    # Find all the PNG files in the directory tree.
    image_paths = []
    for root, dirs, files in os.walk(target_dir):
        for file in files:
            if file[-4:] == ".png":
                image_paths.append(os.path.join(root, file))
    image_paths.sort()

    image_df = pd.DataFrame(data = image_paths, columns = ["Filename"])

    # The generator uses the dataframe of image paths to feed batches of image
    # data to the neural net after rescaling the 8-bit integer pixel values to 
    # floating-point.
    image_generator = tf.keras.preprocessing.image.ImageDataGenerator(
        rescale = 1./255)
    
    predict_gen = image_generator.flow_from_dataframe(
        dataframe = image_df,
        directory = None,
        x_col = "Filename",
        y_col = None,
        target_size = (257, 1000),
        color_mode = 'grayscale',
        batch_size = 16,
        class_mode = None,
        shuffle = False)

    # This will of course spit out the usual warning about the model having not
    # been compiled, which can be safely ignored.
    pnw_cnet_model = tf.keras.models.load_model(model_path)

    # Progress bar is kind of obnoxious when running in RStudio (prints a separate
    # line for each step) but still probably helpful on balance. 
    class_scores = pnw_cnet_model.predict(predict_gen, verbose = 1 if show_prog else 0)

    predictions = pd.DataFrame(data = class_scores, columns = class_names)
    predictions.insert(loc = 0, column = "Filename", 
        value = [os.path.basename(path) for path in image_paths])

    return predictions


# Script can be run through the Python interpreter (has to be in r-reticulate
# or another Python environment / installation with compatible packages) to make
# predictions outside of R. Run
# python pnw-cnet_v4_predict.py E:\Path\to\Data F:\Path\to\PNW-Cnet_v4_TF.h5
try:
    target_dir, model_path = sys.argv[1], sys.argv[2]
    
    print("\nMaking predictions on %s starting at %s.\n" % (target_dir, time.strftime("%H:%M:%S")))
    
    predictions = makePredictions(target_dir, model_path)
    
    print("\nFinished at %s. %d predictions generated.\n" % (time.strftime("%H:%M:%S"), len(predictions)))
    
    output_file = os.path.join(target_dir, "CNN_Predictions_%s.csv" % os.path.basename(target_dir))
    predictions.to_csv(path_or_buf = output_file, index = False)
except:
    pass
    
