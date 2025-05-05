import com.google.gson.*;
import javafx.application.Application;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class BookSearchApp extends Application {
    private ObservableList<String> results = FXCollections.observableArrayList();
    private ListView<String> listView = new ListView<>(results);
    private TextField searchField = new TextField();
    private List<JsonObject> bookObjects = new ArrayList<>(); 

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        Button searchButton = new Button("Search");
        searchButton.setOnAction(e -> searchBooks());
        HBox searchBox = new HBox(10, searchField, searchButton);
        searchBox.setPadding(new Insets(10));
        VBox root = new VBox(10, searchBox, listView);
        root.setPadding(new Insets(10));
        listView.getSelectionModel().selectedIndexProperty().addListener(
            (obs, oldVal, newVal) -> {
                if (newVal.intValue() >= 0 && newVal.intValue() < bookObjects.size()) {
                    showBookDetails(bookObjects.get(newVal.intValue()));
                }
            }
        );
        primaryStage.setScene(new Scene(root, 800, 600));
        primaryStage.setTitle("Kamihate");
        primaryStage.show();
    }

    private void searchBooks() {
        new Thread(() -> {
            try {
                String query = URLEncoder.encode(searchField.getText(), "UTF-8");
                URL url = new URL("https://openlibrary.org/search.json?q=" + query + "&fields=title,author_name,first_publish_year,isbn,number_of_pages_median,subject");
                JsonObject response = JsonParser.parseReader(
                    new InputStreamReader(url.openStream())
                ).getAsJsonObject();

                JsonArray docs = response.getAsJsonArray("docs");

                List<String> titles = new ArrayList<>();
                List<JsonObject> books = new ArrayList<>();
                for (JsonElement doc : docs) {
                    JsonObject book = doc.getAsJsonObject();
                    String title = book.has("title") ? book.get("title").getAsString() : "No Title";
                    titles.add(title);
                    books.add(book);
                }

                javafx.application.Platform.runLater(() -> {
                    results.clear();
                    results.addAll(titles);
                    bookObjects.clear();
                    bookObjects.addAll(books);
                });

            } catch (Exception e) {
                e.printStackTrace();
            }
        }).start();
    }

    private void showBookDetails(JsonObject book) {
        StringBuilder details = new StringBuilder();
        details.append("Title: ").append(book.has("title") ? book.get("title").getAsString() : "N/A").append("\n\n");
        if (book.has("author_name")) {
            details.append("Authors: ").append(String.join(", ", jsonArrayToStringList(book.getAsJsonArray("author_name")))).append("\n\n");
        }
        if (book.has("first_publish_year")) {
            details.append("First Published: ").append(book.get("first_publish_year").getAsInt()).append("\n\n");
        }
        if (book.has("number_of_pages_median")) {
            details.append("Pages: ").append(book.get("number_of_pages_median").getAsInt()).append("\n\n");
        }
        if (book.has("subject")) {
            List<String> subjects = jsonArrayToStringList(book.getAsJsonArray("subject"));
            if (!subjects.isEmpty()) {
                details.append("Subjects: ").append(String.join(", ", subjects.subList(0, Math.min(5, subjects.size())))).append("\n\n");
            }
        }
        if (book.has("isbn")) {
            List<String> isbns = jsonArrayToStringList(book.getAsJsonArray("isbn"));
            if (!isbns.isEmpty()) {
                details.append("ISBN: ").append(isbns.get(0)).append("\n\n");
            }
        }
        javafx.application.Platform.runLater(() -> {
            TextArea detailsArea = new TextArea(details.toString());
            detailsArea.setEditable(false);
            Stage detailsStage = new Stage();
            detailsStage.setScene(new Scene(new ScrollPane(detailsArea), 400, 300));
            detailsStage.setTitle("Specific Book Details");
            detailsStage.show();
        });
    }
    private List<String> jsonArrayToStringList(JsonArray array) {
        List<String> list = new ArrayList<>();
        if (array != null) {
            for (JsonElement element : array) {
                list.add(element.getAsString());
            }
        }
        return list;
    }
}