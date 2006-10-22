code = {' ': "",
        'A': ".-",
        'B': "-...",
        'C': "-.-.",
        'D': "-..",
        'E': ".",
        'F': "..-.",
        'G': "--.",
        'H': "....",
        'I': "..",
        'J': ".---",
        'K': "-.-",
        'L': ".-..",
        'M': "--",
        'N': "-.",
        'O': "---",
        'P': ".--.",
        'Q': "--.-",
        'R': ".-.",
        'S': "...",
        'T': "-",
        'U': "..-",
        'V': "...-",
        'W': ".--",
        'X': "-..-",
        'Y': "-.--",
        'Z': "--..",
        '1': ".----",
        '2': "..---",
        '3': "...--",
        '4': "....-",
        '5': ".....",
        '6': "-....",
        '7': "--...",
        '8': "---..",
        '9': "----.",
        '0': "-----",
        '.': ".-.-.-"}

decode = {}
for letter in code.keys():
    decode[code[letter]] = letter

def text2morse(text):
    message = ""
    for letter in text:
        message += code[letter.upper()]
        message += " "
    return message

def morse2song(message):
    song = []
    for m in message:
        if m == ".":
            song.append( (700, .25) )
            song.append( (0, .25) )
        elif m == "-":
            song.append( (700, .75) )
            song.append( (0, .25) )
        elif m == " ":
            song.append( (0, .75) )
    return song

def morse2text(message):
    text = ""
    current = ""
    for m in message:
        if m == " ":
            text += decode[current]
            current = ""
        else:
            current += m
    return text
