import sys


def main():
    while True:
        message = sys.stdin.readline()
        if message.startswith('stop'): 
            print('Good Bye!!!', flush=True)
            break
        else:
            try:
                print('Evaluating result:', eval(message), flush=True)
            except:
                print('I don\'t understand:', message, flush=True)


if __name__ == '__main__':
    main()
