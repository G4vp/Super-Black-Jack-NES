cards = ["6","5","A", "A"]
score = 0
was_11_used = False
for i in cards:

    if i == "A":
        if score >= 11:
            score += 1
        else:
            score += 11
            was_11_used = True
    elif i in ["10","J","Q","K"]:
        score += 10
    else:
        score += int(i)

    
    if score == 21:
        print('BLACK JACK')
        break
    elif score > 21:
        if was_11_used:
            score -= 11
            score += 1
            was_11_used = False
        else:
            print("You Lost")
            break
        print(score)
    
    print(score)

