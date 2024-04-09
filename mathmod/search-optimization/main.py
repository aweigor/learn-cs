from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


def test_rosenbrock(x1, x2):
    return ( 1 - x1 ) ** 2 + 100 * ( x2 - x1 ** 2 ) ** 2

def test_ellipse(x1, x2):
    A = -2
    B = 4
    return ( x1 / A ) ** 2 + ( x2 / B ) ** 2

def search_zeidel(x1init, x2init, fun):
    x1 = x1init
    x2 = x2init
    h = 8 # постоянная шага
    hmin = 0.000001# минимальный шаг, условие окончания поиска
    x1trace = [x1]
    x2trace = [x2]

    while (h > hmin):
        # спускаемся по первой координате
        # дадим приращение x1 на s, нащупаем улучшение
        # или оставим без изменения
        s = h
        while (fun(x1 + s, x2) > fun(x1, x2)):
            if (s < 0): s = 0
            else: s -= 2 * s

        x1 += s

        # запишем новые значения
        x1trace.append(x1)
        x2trace.append(x2)

        # спускаемся по первой координате
        # дадим приращение x2 на s, нащупаем улучшение
        # или оставим без изменения
        s = h
        while (fun(x1, x2  + s) > fun(x1, x2)):
            if (s < 0): s = 0
            else: s -= 2 * s

        x2 += s

        # запишем новые значения
        x1trace.append(x1)
        x2trace.append(x2)

        # уменьшим шаг, если нет изменений

        print(x1trace[-1], x1)

        if (x1trace[-3] == x1trace[-2] and x2trace[-3] == x2trace[-1]):
            h /= 10

    return x1trace, x2trace


def draw(x1trace, x2trace, fun):

    delta = 0.025
    x = np.arange(-1, 10.0, delta)
    y = np.arange(-1, 10.0, delta)
    X, Y = np.meshgrid(x, y)
    F = fun(X, Y)
    fig, ax = plt.subplots()
    CS = ax.contour(X, Y, F)
    ax.plot(x1trace, x2trace, '-', linewidth=1, color='blue')
    ax.plot(x1trace, x2trace, '-', linewidth=1, color='blue')
    plt.show()

def draw_rosenbrock(x1trace, x2trace, fun):
    n = 100  # number of discretization points along the x-axis
    m = 100  # number of discretization points along the x-axis
    a = -5
    b = 5.  # extreme points in the x-axis
    c = -5
    d = 5.  # extreme points in the y-axis

    X, Y = np.meshgrid(np.linspace(a, b, n), np.linspace(c, d, m))

    Z = fun(X, Y)

    plt.contour(X, Y, Z, np.logspace(-0.5, 3.5, 20, base=10), cmap='gray',  linewidths=0.1)
    plt.plot(x1trace, x2trace, '-', linewidth=1, color='blue')
    plt.plot(x1trace, x2trace, '-', linewidth=1, color='blue')
    plt.xlabel('x1')
    plt.ylabel('x2')
    plt.rc('text', usetex=True)
    plt.rc('font', family='serif')

    plt.show()

def wtiteToCSV(arr1, arr2):
    func_dict = {"x1": x1trace, "x2": x2trace}
    dataframe = pd.DataFrame(func_dict)
    dataframe.to_csv("points.csv")


def writeAsLatexTableRows(arr1, arr2, name, fun):
    for i, val in enumerate(arr1):
        line = "\hline\n"
        line += str(i)
        line += " & "
        line += str(arr1[i])
        line += " & "
        line += str(arr2[i])
        line += " & "
        line += str(fun(arr1[i], arr2[i]))
        with open(name, 'a') as the_file:
            the_file.write(line+' \\\\'+'\n')

def writeAsLatexTableRowsInCount(arr1, arr2, name, fun, count):
    step = len(arr1)//count
    for i in range(count):
        line = "\hline\n"
        line += str(i + 1)
        line += " & "
        line += str(arr1[step*i])
        line += " & "
        line += str(arr2[step*i])
        line += " & "
        line += str(fun(arr1[step*i], arr2[step*i]))
        with open(name, 'a') as the_file:
            the_file.write(line+' \\\\'+'\n')


if __name__ == "__main__":
    x1trace, x2trace = search_zeidel(10, 10, test_ellipse)
    #draw(x1trace, x2trace, test_ellipse)
    writeAsLatexTableRows(x1trace, x2trace, 'table1.txt', test_ellipse)
    #x1trace, x2trace = search_zeidel(-2, 4, test_rosenbrock)
    #writeAsLatexTableRowsInCount(x1trace, x2trace, 'table2.txt', test_rosenbrock, 30)
    #draw_rosenbrock(x1trace, x2trace, test_rosenbrock)
