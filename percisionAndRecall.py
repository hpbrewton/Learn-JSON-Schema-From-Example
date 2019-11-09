import matplotlib.pyplot as plt


pairs2 = {
"1": [1.000000, 0.400000,1.000000, 1],
"2": [1.000000, 0.000000,1.000000, 1],
"3": [1.000000, 0.000000,1.000000, 1],
"4": [1.0, 0.000000,0.750000, 1],
"5": [1.000000, 0.550000,1.000000, 1],
"6": [1.000000, 1.000000,1.000000, 1],
"7": [1.000000, 0.000000,1.000000, 1],
"8": [1.000000, 0.000000,1.000000, 1],
"9": [1.000000, 0.050000,1.000000, 1]
}

# rdata = [pairs[0] for i in range(len(pairs))]
# odata = [pairs[1] for i in range(len(pairs))]

fig, axs = plt.subplots(2, 1, figsize=(1, 2), sharex = True)

data = pairs2
names = [label for label in data.keys() for i in range(2)]
values = [v for label in data.keys() for v in data[label][2:]]
axs[0].scatter(names, values, label="a")

data = pairs2
names = [label for label in data.keys() for i in range(2)]
values = [v for label in data.keys() for v in data[label][:2]]
axs[1].scatter(names, values, label="B")

fig.suptitle('Categorical Plotting')
plt.show()