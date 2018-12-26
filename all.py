from __future__ import print_function
import math

from IPython import display
from matplotlib import cm
from matplotlib import gridspec
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import pandas as pd
from sklearn import metrics
import tensorflow as tf
from tensorflow import keras
import keras
from tensorflow.keras import layers

from tensorflow.python.data import Dataset

tf.logging.set_verbosity(tf.logging.ERROR)
pd.options.display.max_rows = 10
pd.options.display.max_columns = 50

pd.options.display.float_format = '{:.1f}'.format



equipment_success_dataframe = pd.read_csv("./equipment_success_unique.csv", sep=",")

equipment_success_dataframe['years_old'] = round(equipment_success_dataframe['age_in_months_at_equipped_start'] / 12)

def isMale(x):
    if x == "male":
        return 1
    return 0
def isFemale(x):
    if x == "female":
        return 1
    return 0
def isRightEar(x):
    if x == "R":
        return 1
    return 0
def isLeftEar(x):
    if x == "L":
        return 1
    return 0
def isBothEars(x):
    if x == "BIN":
        return 1
    return 0
def dbToAmp (x):
    return 10**(x/20)


equipment_success_dataframe['isMale'] = equipment_success_dataframe['gender'].apply(isMale)
equipment_success_dataframe['isFemale'] = equipment_success_dataframe['gender'].apply(isFemale)
equipment_success_dataframe['isRightEar'] = equipment_success_dataframe['ears'].apply(isRightEar)
equipment_success_dataframe['isLeftEar'] = equipment_success_dataframe['ears'].apply(isLeftEar)
equipment_success_dataframe['isBothEars'] = equipment_success_dataframe['ears'].apply(isBothEars)

equipment_success_dataframe['freq_500_over_freq_1000'] = equipment_success_dataframe['freq_500']/equipment_success_dataframe['freq_1000']
equipment_success_dataframe['freq_2000_over_freq_1000'] = equipment_success_dataframe['freq_2000']/equipment_success_dataframe['freq_1000']
equipment_success_dataframe['freq_4000_over_freq_1000'] = equipment_success_dataframe['freq_4000']/equipment_success_dataframe['freq_1000']


equipment_success_dataframe['freq_500_gain'] = equipment_success_dataframe['freq_500'] - equipment_success_dataframe['eq_freq_500']
equipment_success_dataframe['freq_1000_gain'] = equipment_success_dataframe['freq_1000'] - equipment_success_dataframe['eq_freq_1000']
equipment_success_dataframe['freq_2000_gain'] = equipment_success_dataframe['freq_2000'] - equipment_success_dataframe['eq_freq_2000']
equipment_success_dataframe['freq_4000_gain'] = equipment_success_dataframe['freq_4000'] - equipment_success_dataframe['eq_freq_4000']

equipment_success_dataframe['freq_500_gain_ratio'] = equipment_success_dataframe['freq_500_gain'] / equipment_success_dataframe['freq_500']
equipment_success_dataframe['freq_1000_gain_ratio'] = equipment_success_dataframe['freq_1000_gain'] / equipment_success_dataframe['freq_1000']
equipment_success_dataframe['freq_2000_gain_ratio'] = equipment_success_dataframe['freq_2000_gain'] / equipment_success_dataframe['freq_2000']
equipment_success_dataframe['freq_4000_gain_ratio'] = equipment_success_dataframe['freq_4000_gain'] / equipment_success_dataframe['freq_4000']

# equipment_success_dataframe['eq_gain_ratio'] = (-equipment_success_dataframe['eq_average_loss'] + equipment_success_dataframe['average_loss'])/equipment_success_dataframe['average_loss']


equipment_success_dataframe = equipment_success_dataframe.drop(columns=["eq_average_loss", "eq_high_loss", "eq_low_loss", "rk", "ears", "gender"])

equipment_success_dataframe

equipment_success_dataframe['valid'] = (
        equipment_success_dataframe['freq_250'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_500'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_750'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_1000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_1500'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_2000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_3000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_4000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_6000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['freq_8000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['eq_freq_500'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['eq_freq_1000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['eq_freq_2000'].apply(lambda x: not math.isnan(x)) &
        equipment_success_dataframe['eq_freq_4000'].apply(lambda x: not math.isnan(x)) &

        equipment_success_dataframe['freq_500_gain'].apply(lambda x: not math.isnan(x) and x > 0) &
        equipment_success_dataframe['freq_1000_gain'].apply(lambda x: not math.isnan(x) and x > 0) &
        equipment_success_dataframe['freq_2000_gain'].apply(lambda x: not math.isnan(x) and x > 0) &
        equipment_success_dataframe['freq_4000_gain'].apply(lambda x: not math.isnan(x) and x > 0) &

        equipment_success_dataframe['freq_500_gain_ratio'].apply(lambda x: not math.isnan(x) and x < 1) &
        equipment_success_dataframe['freq_1000_gain_ratio'].apply(lambda x: not math.isnan(x) and x < 1) &
        equipment_success_dataframe['freq_2000_gain_ratio'].apply(lambda x: not math.isnan(x) and x < 1) &
        equipment_success_dataframe['freq_4000_gain_ratio'].apply(lambda x: not math.isnan(x) and x < 1) &

        equipment_success_dataframe['isBothEars'].apply(lambda x: x == 0) &
        equipment_success_dataframe['isMale'].apply(lambda x: x == 1) &
        equipment_success_dataframe['average_loss'].apply(lambda x: not math.isnan(x)) &
        #     equipment_success_dataframe['high_loss'].apply(lambda x : not math.isnan(x)) &
        #     equipment_success_dataframe['low_loss'].apply(lambda x : not math.isnan(x)) &
        equipment_success_dataframe['months_since_equipped'].apply(lambda x: x > 12) &
        equipment_success_dataframe['years_old'].apply(lambda x: not math.isnan(x) and 30 < x < 100)
)


#equipment_success = equipment_success_dataframe.query('center_id == 29 and valid == True').copy()
equipment_success_valid = equipment_success_dataframe.query(' valid == True').copy()

equipment_success_valid



enice = equipment_success_valid.loc[:, ['eq_freq_2000', 'freq_250', 'freq_500', 'freq_750', 'freq_1000', 'freq_1500', 'freq_2000', 'freq_3000', 'freq_4000', 'freq_6000', 'freq_8000', 'years_old']]
enice['250_500'] = enice['freq_250'] - enice['freq_500']
enice['500_750'] = enice['freq_500'] - enice['freq_750']
enice['750_1000'] = enice['freq_750'] - enice['freq_1000']
enice['1000_1500'] = enice['freq_1000'] - enice['freq_1500']
enice['1500_2000'] = enice['freq_1500'] - enice['freq_2000']
enice['2000_3000'] = enice['freq_2000'] - enice['freq_3000']
enice['3000_4000'] = enice['freq_3000'] - enice['freq_4000']
enice['4000_6000'] = enice['freq_4000'] - enice['freq_6000']
enice['6000_8000'] = enice['freq_6000'] - enice['freq_8000']
equipment_success_valid = enice

enice



def scaleInner(df, feature):
    stats = df[feature].describe();
    std = stats['std']
    mean = stats['mean']
    df[feature + '_scaled'] = (df[feature] - mean)/std
    return df

def scale(df, features):
    scaledDf = df.copy()
    for feature in features:
        scaleInner(scaledDf, feature)
    return scaledDf

def success_ratio(equipment_success, freq, multiplier, threshold ):
    data = pd.value_counts(
        equipment_success['freq_' + freq + '_gain'] > (equipment_success['freq_' + freq] * multiplier) - threshold
    )
    return data[True]/(data[False] + data[True])

def model_good_prediction_ratio(targets, predictions, max_distance):
    predictionsDiff = (targets - predictions).apply (lambda x: abs(x))
    return len(list(filter(lambda x: x < max_distance, predictionsDiff)))/len(predictionsDiff)

equipment_success = equipment_success_valid.copy() #[(equipment_success_valid['isMale'] == 0)
                                           # & (equipment_success_valid['decades_old'] == 7)
                                           #].copy()

equipment_success


from keras.models import Sequential, Model
from keras.layers import Dense, Activation, Input, Dropout
from keras import optimizers



def model_good_prediction_ratio_array(targets, predictions, max_distance):
    predictionsDiff = list(map(lambda x: abs(x), list(targets - predictions)))
    return len(list(filter(lambda x: x < max_distance, predictionsDiff)))/len(predictionsDiff)


equipment_success_valid = scale(equipment_success_valid, ['freq_250', 'freq_500', 'freq_750', 'freq_1000',
                                                          'freq_1500', 'freq_2000', 'freq_3000', 'freq_4000',
                                                          'freq_6000', 'freq_8000', 'years_old', '250_500',
    '500_750',
    '750_1000',
    '1000_1500',
    '1500_2000',
    '2000_3000',
    '3000_4000',
    '4000_6000',
    '6000_8000'])

equipment_success_all = equipment_success_valid.copy()
equipment_success = equipment_success_all.sample(frac=1)
equipment_success.describe()


features = [
   "years_old_scaled",
    'freq_250_scaled',
    'freq_500_scaled',
    'freq_750_scaled',
    'freq_1000_scaled',
    'freq_1500_scaled',
    'freq_2000_scaled',
    'freq_3000_scaled',
    'freq_4000_scaled',
    'freq_6000_scaled',
    'freq_8000_scaled',
    '250_500_scaled',
    '500_750_scaled',
    '750_1000_scaled',
    '1000_1500_scaled',
    '1500_2000_scaled',
    '2000_3000_scaled',
    '3000_4000_scaled',
    '4000_6000_scaled',
    '6000_8000_scaled'
]

data = (equipment_success[features].values)
labels = (equipment_success['eq_freq_2000'].values)

model_good_prediction_ratio_array(labels, np.average(labels), 5)



from keras.callbacks import EarlyStopping

model = Sequential([
    Dense(30, input_dim=len(features), activation='relu',  kernel_initializer='normal'),
    Dropout(0.2),
    Dense(15, activation='relu',  kernel_initializer='normal'),
    Dense(15, activation='softmax',  kernel_initializer='normal'),
    Dense(1, activation='linear')
])

earlystopper = EarlyStopping(monitor='val', patience=5, verbose=1)

model.compile(optimizer='adam', loss='mse', metrics=['mse'])
history = model.fit(data, labels, epochs=2000, batch_size=None,steps_per_epoch=30,
          validation_split=0.2,  validation_steps=100, verbose=1, callbacks=[earlystopper])


db_threshold = 5
predictions = model.predict(data).transpose()[0]
print('Predictions:\t {:2.0f}% vs \nBaseline:\t {:2.0f}%'.format(
    model_good_prediction_ratio_array(labels, predictions, db_threshold)*100,
    model_good_prediction_ratio_array(labels, np.average(labels), db_threshold)*100
))



# Plot training & validation loss values
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('Model loss')
plt.ylabel('Loss')
plt.ylim(0, 100)
plt.xlabel('Epoch')
plt.legend(['Train', 'Validation'], loc='upper right')
plt.show()

