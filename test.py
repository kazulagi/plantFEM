import pyaudio
import numpy as np
import math

CHUNK = 4096
RATE = 44100  # サンプリングレート [Hz]

def tone(freq, length, gain):
    """
    指定した周波数の定常波を作成

    Parameters
    ----------
    freq   : 周波数 [Hz]
    length : 長さ [s]
    gain   : 大きさ
    """
    t = np.arange(int(length * RATE)) / RATE
    return np.sin(t * float(freq) * np.pi * 2) * gain


# 半音上がる際の周波数変化割合
r_semitone = math.pow(2, 1.0/12)
# ド〜ドの12+1音階の周波数
scale_hz = [261.625]
for _ in range(12):
    scale_hz.append(scale_hz[-1] * r_semitone)

p = pyaudio.PyAudio()
stream_out = p.open(
    format = pyaudio.paFloat32,
    channels = 1,
    rate = RATE,
    frames_per_buffer = CHUNK,
    input = True,
    output = True
)


# ドレミファソラシド
for i in [0, 2, 4, 5, 7, 9, 11, 12]:
    sound = tone(scale_hz[i], 0.5, 1.0).astype(np.float32).tostring()
    stream_out.write(sound)

# 小さい音で
for i in [0, 2, 4, 5, 7, 9, 11, 12]:
    sound = tone(scale_hz[i], 0.5, 0.2).astype(np.float32).tostring()
    stream_out.write(sound)

# 鳴らす時間を短く
for i in [0, 2, 4, 5, 7, 9, 11, 12]:
    sound = tone(scale_hz[i], 0.2, 1.0).astype(np.float32).tostring()
    stream_out.write(sound)

stream_out.close()
p.terminate()