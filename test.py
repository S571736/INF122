def print_diamond(N):
    print('Tall:', N)

    # Midterste linje er N*2 brei?
    rader = N # + (N - 1)
    spaces = 0
    kryss = 0
    
    # Hvis det er bare ei stjerne
    if rader == 1
        print("X") 
    else:
        # Lager kryssene fram til midten av diamanten
        for i in range(rader): 
            # lager mellomrommene fram til krysset
            for j in range(rader - i):

                print(end=" ") # TODO:lag to mellomrom for å test

            # Ukjent
            kryss = kryss - 1 # TODO: Kafaen gjør denne? slett for å se etterpå

            # Skriver kryssene
            for j in range((2*i)-1):
                print('X', end="")

            #Går videre til neste linje
            print()

        kryss = rader - 2 # TODO: wtf? test uten

        # lager kryssene fra midten til slutten
        for i in range(rader, 0, -1):
            # Lager mellomrommene fram til kryssene begynner
            for j in range(spaces + 1):
                print(end=" ")
            spaces += 1

            # Ukjent
            kryss = kryss + 1 # TODO: Kafaen gjør denne? slett for å se etterpå

            for j in range(i + 1):
                print("X", end="")
            print()


if __name__ == '__main__':
    print_diamond(1)
    print()
    
    print_diamond(2)
    print()
    
    print_diamond(3)
    print()
    
    print_diamond(4)
    print()
    
    print_diamond(10)
    print()
        
    