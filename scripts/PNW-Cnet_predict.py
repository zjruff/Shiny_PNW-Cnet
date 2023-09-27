# 06 Sep 2023
# Script to classify spectrograms using PNW-Cnet v4 or v5. Meant to be read into 
# R using the reticulate::source_python(), allowing the makePredictions function
# to be used in R, but can also be run as a standalone script by supplying the
# path to the PNW-Cnet model file and the path to the folder containing spectrogram
# images as command line arguments, e.g.
# python PNW-Cnet_predict.py E:\Path\to\PNW-Cnet_v5_TF.h5 F:\Path\to\images

import math, os, sys, time
import pandas as pd
import tensorflow as tf

# Returns a Pandas DataFrame, which R can handle more or less natively. The
# DataFrame has one column Filename for the names of the image files (not full
# paths) and either 51 (PNW-Cnet v4) or 135 (PNW-Cnet v5) columns containing 
# class scores.
def makePredictions(target_dir, model_path, show_prog = False):
    v4_class_names = ['AEAC', 'BRCA', 'BRMA', 'BUVI', 'CAGU', 'CALU', 'CAUS', 'CCOO', 
                    'CHFA', 'CHMI', 'CHMI_IRREG', 'COAU', 'COAU2', 'COCO', 'CYST', 
                    'DEFU', 'DOG', 'DRPU', 'DRUM', 'FLY', 'FROG', 'GLGN', 'HOSA', 
                    'HYPI', 'INSP', 'IXNA', 'MEKE', 'MYTO', 'NUCO', 'OCPR', 'ORPI', 
                    'PAFA', 'PECA', 'PHNU', 'PIMA', 'POEC', 'PSFL', 'SHOT', 'SITT', 
                    'SPRU', 'STOC', 'STOC_IRREG', 'STVA', 'STVA_IRREG', 'TADO1', 
                    'TADO2', 'TAMI', 'TUMI', 'WHIS', 'YARD', 'ZEMA']
                    
    v5_class_names = ['ACCO1', 'ACGE1', 'ACGE2', 'ACST1', 'AEAC1', 'AEAC2', 
                    'Airplane', 'ANCA1', 'ASOT1', 'BOUM1', 'BRCA1', 'BRMA1', 
                    'BRMA2', 'BUJA1', 'BUJA2', 'Bullfrog', 'BUVI1', 'BUVI2', 
                    'CACA1', 'CAGU1', 'CAGU2', 'CAGU3', 'CALA1', 'CALU1', 
                    'CAPU1', 'CAUS1', 'CAUS2', 'CCOO1', 'CCOO2', 'CECA1', 
                    'Chainsaw', 'CHFA1', 'Chicken', 'CHMI1', 'CHMI2', 'COAU1',
                    'COAU2', 'COBR1', 'COCO1', 'COSO1', 'Cow', 'Creek', 
                    'Cricket', 'CYST1', 'CYST2', 'DEFU1', 'DEFU2', 'Dog', 
                    'DRPU1', 'Drum', 'EMDI1', 'EMOB1', 'FACO1', 'FASP1', 
                    'Fly', 'Frog', 'GADE1', 'GLGN1', 'Growler', 'Gunshot', 
                    'HALE1', 'HAPU1', 'HEVE1', 'Highway', 'Horn', 'Human', 
                    'HYPI1', 'IXNA1', 'IXNA2', 'JUHY1', 'LEAL1', 'LECE1', 
                    'LEVI1', 'LEVI2', 'LOCU1', 'MEFO1', 'MEGA1', 'MEKE1', 
                    'MEKE2', 'MEKE3', 'MYTO1', 'NUCO1', 'OCPR1', 'ODOC1', 
                    'ORPI1', 'ORPI2', 'PAFA1', 'PAFA2', 'PAHA1', 'PECA1', 
                    'PHME1', 'PHNU1', 'PILU1', 'PILU2', 'PIMA1', 'PIMA2', 
                    'POEC1', 'POEC2', 'PSFL1', 'Rain', 'Raptor', 'SICU1', 
                    'SITT1', 'SITT2', 'SPHY1', 'SPHY2', 'SPPA1', 'SPPI1', 
                    'SPTH1', 'STDE1', 'STNE1', 'STNE2', 'STOC_4Note', 
                    'STOC_Series', 'Strix_Bark', 'Strix_Whistle', 'STVA_8Note', 
                    'STVA_Insp', 'STVA_Series', 'Survey_Tone', 'TADO1', 'TADO2',
                    'TAMI1', 'Thunder', 'TRAE1', 'Train', 'Tree', 'TUMI1', 
                    'TUMI2', 'URAM1', 'VIHU1', 'Wildcat', 'Yarder', 'ZEMA1', 
                    'ZOLE1']

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
    # floating-point in the range [0,1].
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

    # Spits out a few informational and warning messages which can be safely ignored.
    pnw_cnet_model = tf.keras.models.load_model(model_path)

    # Progress bar is kind of obnoxious when running in RStudio (prints a separate
    # line for each step) but still probably helpful on balance. 
    class_scores = pnw_cnet_model.predict(predict_gen, verbose = 1 if show_prog else 0)

    # Function applies different column labels depending on the number of columns
    # (i.e., target classes) in the class_scores dataframe. If it gets an unexpected
    # number of columns (neither 51 nor 135) it just labels them sequentially.
    n_cols = class_scores.shape[1]
    if n_cols == 51:
        score_cols = v4_class_names
    elif n_cols == 135:
        score_cols = v5_class_names
    else:
        print("Warning: Prediction dataframe has unexpected number of columns. Cannot determine class names.")
        score_cols = ["Class_%s" % str(i).zfill(int(math.log10(n_cols))+1) for i in range(1, n_cols + 1)]

    predictions = pd.DataFrame(data = class_scores, columns = score_cols).round(decimals = 5)
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
    
    predictions = makePredictions(target_dir, model_path, show_prog = True)
    
    print("\nFinished at %s. %d predictions generated.\n" % (time.strftime("%H:%M:%S"), len(predictions)))
    
    output_file = os.path.join(target_dir, "CNN_Predictions_%s.csv" % os.path.basename(target_dir))
    predictions.to_csv(path_or_buf = output_file, index = False)
except:
    pass
