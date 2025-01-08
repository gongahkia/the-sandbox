import toga
from toga.style import Pack
from toga.style.pack import COLUMN, ROW

class MyApp(toga.App):
    def startup(self):

        main_box = toga.Box(style=Pack(direction=COLUMN))

        self.label = toga.Label("Hello, Briefcase!", style=Pack(padding=(0, 5)))

        button = toga.Button(
            "Click Me",
            on_press=self.on_button_click,
            style=Pack(padding=5)
        )

        main_box.add(self.label)
        main_box.add(button)

        self.main_window = toga.MainWindow(title=self.name)
        self.main_window.content = main_box
        self.main_window.show()

    def on_button_click(self, widget):
        self.label.text = "Button Clicked!"

def main():
    return MyApp("My Sample Briefcase App by @gongahkia", "org.example.myapp")

if __name__ == "__main__":
    main().main_loop()