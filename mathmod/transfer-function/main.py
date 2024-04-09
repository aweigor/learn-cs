from matplotlib import pyplot as plt
import pandas as pd

def transfer_function():
    z_1 = [0]
    z_2 = [0]
    step = 0.001
    h = step
    t = [h]
    y = [0]

    for i in range(1, 500):
        z_1.append(round(z_1[i - 1] + h * z_2[i - 1], 5))
        z_2.append(round(z_2[i - 1] - h * ((z_1[i - 1] + (2.4 * z_2[i - 1]) - 5) / 9), 5))
        y.append(round(2 * z_1[i - 1] + 10 * z_2[i - 1], 5))
        h += step
        t.append(round(h, 5))

    func_dict = {"t": t, "y": y}
    dataframe = pd.DataFrame(func_dict)
    dataframe.to_csv("points.csv")

    df = pd.read_csv("points.csv")
    x = df["t"]
    y = df["y"]
    plt.xlabel("t")
    plt.ylabel("y(t)")

    plt.plot(x,y)
    plt.savefig("graph.png")


if __name__ == "__main__":
    transfer_function()