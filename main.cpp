#include <algorithm>
#include <cctype>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <functional>
#include <optional>
#include <string>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#include <utility>
#include <vector>

const std::string KILO_VERSION = "0.0.1";
const int KILO_TAB_STOP = 8;
const int KILO_QUIT_TIMES = 3;

/*** defines ***/

#define CTRL_KEY(k) ((k)&0x1f)

// 素直に普通のenumにすべきだった気がする...
enum class EditorKey : int {
    Backspace = 127,
    ArrowLeft = 1000,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    PageUp,
    PageDown,
    HomeKey,
    EndKey,
    DelKey,
};

enum class EditorHighlight {
    Normal,
    Comment,
    MlComment,
    Keyword1,
    Keyword2,
    String,
    Number,
    Match,
};

const int HL_HIGHLIGHT_NUMBERS = 1 << 0;
const int HL_HIGHLIGHT_STRINGS = 1 << 1;

/*** data ***/

struct EditorSyntax {
    std::string filetype;
    std::vector<std::string> filematch;
    std::vector<std::string> keywords;
    std::string singleline_comment_start;
    std::string multiline_comment_start;
    std::string multiline_comment_end;
    int flags;
};

struct Erow {
    int idx;
    std::string chars;
    std::string render;
    std::vector<EditorHighlight> hl;
    bool hl_open_comment;
};

struct EditorConfig {
    int cx, cy;
    int rx;
    int rowoff;
    int coloff;
    int screenrows;
    int screencols;
    int numrows;
    std::vector<Erow> row;
    bool dirty;
    std::optional<std::string> filename;
    std::string statusmsg;
    std::time_t statusmsg_time;
    std::optional<EditorSyntax> syntax;
    termios orig_termios;
};

EditorConfig E;

/*** filetypes ***/

std::vector<std::string> C_HL_extensions = {".c", ".h", ".cpp"};

std::vector<std::string> C_HL_keywords = {
    "switch", "if",    "while",   "for",    "break", "continue",  "return",  "else",
    "struct", "union", "typedef", "static", "enum",  "class",     "case",

    "int|",   "long|", "double|", "float|", "char|", "unsigned|", "singed|", "void|",
};

std::vector<EditorSyntax> HLDB = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//",
        "/*",
        "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
    },
};

/*** prototypes ***/

void editor_set_status_message(const char *fmt, ...);
void editor_refresh_screen();
std::optional<std::string> editor_prompt(
    const std::string &prompt,
    const std::optional<std::function<void(const std::string &, int)>> &callback = std::nullopt);

/*** terminal ***/

void die(const std::string &s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    std::perror(s.c_str());
    std::exit(1);
}

void disable_raw_mode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
        die("tcsetattr");
}

void enable_raw_mode() {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
        die("tcgetattr");
    std::atexit(disable_raw_mode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= CS8;
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
        die("tcsetattr");
}

EditorKey editor_read_key() {
    EditorKey x1b = static_cast<EditorKey>('\x1b');
    char c;
    int nread;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN)
            die("read");
    }
    if (c == '\x1b') {
        std::string seq;
        seq.resize(3);
        if (read(STDIN_FILENO, &seq[0], 1) != 1)
            return x1b;
        if (read(STDIN_FILENO, &seq[1], 1) != 1)
            return x1b;
        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1)
                    return x1b;
                if (seq[2] == '~') {
                    switch (seq[1]) {
                    case '1':
                    case '7':
                        return EditorKey::HomeKey;
                    case '4':
                    case '8':
                        return EditorKey::EndKey;
                    case '3':
                        return EditorKey::DelKey;
                    case '5':
                        return EditorKey::PageUp;
                    case '6':
                        return EditorKey::PageDown;
                    }
                }
            } else {
                switch (seq[1]) {
                case 'A':
                    return EditorKey::ArrowUp;
                case 'B':
                    return EditorKey::ArrowDown;
                case 'C':
                    return EditorKey::ArrowRight;
                case 'D':
                    return EditorKey::ArrowLeft;
                case 'H':
                    return EditorKey::HomeKey;
                case 'F':
                    return EditorKey::EndKey;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
            case 'H':
                return EditorKey::HomeKey;
            case 'F':
                return EditorKey::EndKey;
            }
        }
        return x1b;
    } else {
        return static_cast<EditorKey>(c);
    }
}

std::optional<std::pair<int, int>> get_cursur_position() {
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4)
        return std::nullopt;

    char c;
    std::string buf;
    while (read(STDIN_FILENO, &c, 1) == 1) {
        if (c == 'R')
            break;
        buf += c;
    }

    if (buf[0] != '\x1b' || buf[1] != '[')
        return std::nullopt;

    int rows, cols;
    if (std::sscanf(buf.c_str() + 2, "%d;%d", &rows, &cols) != 2)
        return std::nullopt;

    return std::make_pair(rows, cols);
}

std::optional<std::pair<int, int>> get_window_size() {
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)
            return std::nullopt;
        return get_cursur_position();
    } else {
        return std::make_pair(ws.ws_row, ws.ws_col);
    }
}

/*** syntax highlighting ***/

bool is_separator(char c) {
    static const std::string s = ",.()+-/*=~%<>[];\"";
    return std::isspace(c) || c == '\0' || s.find(c) != std::string::npos;
}

void editor_update_syntax(Erow &row) {
    row.hl.assign(row.render.size(), EditorHighlight::Normal);

    if (!E.syntax.has_value())
        return;
    auto syntax = E.syntax.value();

    std::vector<std::string> keywords = syntax.keywords;

    std::string scs = syntax.singleline_comment_start;
    std::string mcs = syntax.multiline_comment_start;
    std::string mce = syntax.multiline_comment_end;

    bool prev_sep = true;
    std::optional<char> in_string;
    bool in_comment = (row.idx > 0 && E.row[row.idx - 1].hl_open_comment);
    int i = 0;
    while (i < int(row.render.size())) {
        char c = row.render[i];
        EditorHighlight prev_hl = (i > 0) ? row.hl[i - 1] : EditorHighlight::Normal;

        if (!scs.empty() && !in_string.has_value() && !in_comment) {
            if (row.render.substr(i, scs.size()) == scs) {
                std::fill(row.hl.begin() + i, row.hl.end(), EditorHighlight::Comment);
                break;
            }
        }

        if (!mcs.empty() && !mce.empty() && !in_string.has_value()) {
            if (in_comment) {
                row.hl[i] = EditorHighlight::MlComment;
                if (row.render.substr(i, mce.size()) == mce) {
                    std::fill(row.hl.begin() + i, row.hl.begin() + i + mce.size(),
                              EditorHighlight::MlComment);
                    i += mce.size();
                    in_comment = false;
                    prev_sep = true;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (row.render.substr(i, mcs.size()) == mcs) {
                std::fill(row.hl.begin() + i, row.hl.begin() + i + mcs.size(),
                          EditorHighlight::MlComment);
                i += mcs.size();
                in_comment = true;
                continue;
            }
        }

        if (syntax.flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string.has_value()) {
                row.hl[i] = EditorHighlight::String;
                if (c == '\\' && i + 1 < int(row.render.size())) {
                    row.hl[i + 1] = EditorHighlight::String;
                    i += 2;
                    continue;
                }
                if (c == in_string.value())
                    in_string.reset();
                prev_sep = true;
                i++;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row.hl[i] = EditorHighlight::String;
                    i++;
                    continue;
                }
            }
        }

        if (syntax.flags & HL_HIGHLIGHT_NUMBERS) {
            if ((std::isdigit(c) && (prev_sep || prev_hl == EditorHighlight::Number)) ||
                (c == '.' && prev_hl == EditorHighlight::Number)) {
                row.hl[i] = EditorHighlight::Number;
                prev_sep = false;
                i++;
                continue;
            }
        }

        if (prev_sep) {
            bool k = false;
            for (auto keyword : keywords) {
                bool kw2 = keyword.back() == '|';
                if (kw2)
                    keyword.pop_back();

                if (row.render.substr(i, keyword.size()) == keyword &&
                    ((i + keyword.size() < row.render.size()) ||
                     (is_separator(row.render[i + keyword.size()])))) {
                    std::fill(row.hl.begin() + i, row.hl.begin() + i + keyword.size(),
                              (kw2 ? EditorHighlight::Keyword2 : EditorHighlight::Keyword1));
                    i += keyword.size();
                    k = true;
                    break;
                }
            }
            if (k) {
                prev_sep = false;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    bool changed = (row.hl_open_comment != in_comment);
    row.hl_open_comment = in_comment;
    if (changed && row.idx + 1 < E.numrows)
        editor_update_syntax(E.row[row.idx + 1]);
}

int editor_syntax_to_color(EditorHighlight hl) {
    switch (hl) {
    case EditorHighlight::Comment:
    case EditorHighlight::MlComment:
        return 36;
    case EditorHighlight::Keyword1:
        return 33;
    case EditorHighlight::Keyword2:
        return 32;
    case EditorHighlight::String:
        return 35;
    case EditorHighlight::Number:
        return 31;
    case EditorHighlight::Match:
        return 34;
    default:
        return 37;
    }
}

void editor_select_syntax_highlight() {
    E.syntax.reset();
    if (!E.filename.has_value())
        return;

    static auto get_ext = [](const std::string &filename) -> std::optional<std::string> {
        auto pos = filename.rfind(".");
        if (pos == std::string::npos)
            return std::nullopt;
        else
            return filename.substr(pos);
    };

    auto ext = get_ext(E.filename.value());
    for (const auto &h : HLDB) {
        for (const auto &m : h.filematch) {
            bool is_ext = (!m.empty() && m[0] == '.');
            if ((is_ext && ext.has_value() && ext == m) || (!is_ext && E.filename.value() == m)) {
                E.syntax = h;

                for (int filerow = 0; filerow < E.numrows; filerow++)
                    editor_update_syntax(E.row[filerow]);
                return;
            }
        }
    }
}

/*** row operations ***/

int editor_row_cx_to_rx(const Erow &row, int cx) {
    int rx = 0;
    for (int i = 0; i < cx; i++) {
        if (row.chars[i] == '\t')
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        rx++;
    }
    return rx;
}

int editor_row_rx_to_cx(const Erow &row, int rx) {
    int cur_rx = 0;
    int cx = 0;
    for (; cx < int(row.chars.size()); cx++) {
        if (row.chars[cx] == '\t')
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        cur_rx++;

        if (cur_rx > rx)
            return cx;
    }

    return cx;
}

void editor_update_row(Erow &row) {
    std::string render;
    for (int i = 0; i < int(row.chars.size()); i++) {
        if (row.chars[i] == '\t') {
            render.push_back(' ');
            while (render.size() % KILO_TAB_STOP != 0) {
                render.push_back(' ');
            }
        } else {
            render.push_back(row.chars[i]);
        }
    }
    row.render = render;

    editor_update_syntax(row);
}

void editor_insert_row(int at, const std::string &s) {
    if (at < 0 || at > E.numrows)
        return;

    auto it = E.row.begin() + at;
    E.row.insert(it, Erow{at, s, "", {}, false});
    for (int j = at + 1; j <= E.numrows; j++)
        E.row[j].idx++;

    editor_update_row(E.row[at]);

    E.numrows++;
    E.dirty = true;
}

void editor_del_row(int at) {
    if (at < 0 || at >= E.numrows)
        return;
    E.row.erase(E.row.begin() + at);
    for (int j = at; j < E.numrows - 1; j++)
        E.row[j].idx--;
    E.numrows--;
    E.dirty = true;
}

void editor_row_insert_char(Erow &row, int at, int c) {
    if (at < 0 || at > int(row.chars.size()))
        row.chars.push_back(c);
    else
        row.chars.insert(at, std::string{char(c)});
    editor_update_row(row);
    E.dirty = true;
}

void editor_row_del_char(Erow &row, int at) {
    if (at < 0 || at >= int(row.chars.size()))
        return;
    row.chars.erase(at, 1);
    editor_update_row(row);
    E.dirty = true;
}

/*** editor operations ***/

void editor_insert_char(int c) {
    if (E.cy == E.numrows)
        editor_insert_row(E.numrows, "");
    editor_row_insert_char(E.row[E.cy], E.cx, c);
    E.cx++;
}

void editor_insert_newline() {
    if (E.cx == 0) {
        editor_insert_row(E.cy, "");
    } else {
        // 後ろにinsertしても参照が無効になる...?
        // Erow &row = E.row.at(E.cy);
        if (E.cx < int(E.row.at(E.cy).chars.size())) {
            editor_insert_row(E.cy + 1, E.row.at(E.cy).chars.substr(E.cx));
            E.row.at(E.cy).chars.erase(E.cx);
        } else {
            editor_insert_row(E.cy + 1, "");
        }
        editor_update_row(E.row.at(E.cy));
    }
    E.cy++;
    E.cx = 0;
}

void editor_row_append_string(Erow &row, const std::string &s) {
    row.chars += s;
    editor_update_row(row);
    E.dirty = true;
}

void editor_del_char() {
    if (E.cy == E.numrows)
        return;
    if (E.cx == 0 && E.cy == 0)
        return;

    Erow &row = E.row[E.cy];
    if (E.cx > 0) {
        editor_row_del_char(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.row[E.cy - 1].chars.size();
        editor_row_append_string(E.row[E.cy - 1], row.chars);
        editor_del_row(E.cy);
        E.cy--;
    }
}

/*** file i/o ***/

std::string editor_rows_to_string() {
    std::string buf;
    for (const auto &row : E.row) {
        buf += row.chars + "\n";
    }
    return buf;
}

void editor_open(const std::string &filename) {
    E.filename = filename;

    editor_select_syntax_highlight();

    std::ifstream ifs(filename);
    if (ifs.fail())
        die("ifstream");

    std::string line;
    while (std::getline(ifs, line)) {
        while (line[line.size() - 1] == '\n' || line[line.size() - 1] == '\r')
            line.pop_back();
        editor_insert_row(E.numrows, line);
    }
    E.dirty = false;
}

void editor_save() {
    if (!E.filename.has_value()) {
        auto filename = editor_prompt("Save as: %s (ESC to cancel)");
        if (!filename.has_value()) {
            editor_set_status_message("Save aborted");
            return;
        }
        E.filename = filename.value();
        editor_select_syntax_highlight();
    }

    std::string buf = editor_rows_to_string();
    std::ofstream out(E.filename.value());
    out << buf;
    editor_set_status_message("%d bytes written to disk", int(buf.size()));
    E.dirty = false;
}

/*** find ***/

void editor_find() {
    static auto callback = [](const std::string &query, int c) {
        enum class Dir { Up, Down };
        static std::optional<int> last_match = std::nullopt;
        static Dir direction = Dir::Down;

        static std::optional<std::pair<int, std::vector<EditorHighlight>>> saved_hl;

        if (saved_hl.has_value()) {
            auto [line, hl] = saved_hl.value();
            E.row[line].hl = hl;
            saved_hl.reset();
        }

        EditorKey key = static_cast<EditorKey>(c);
        if (c == '\r' || c == '\x1b') {
            last_match = std::nullopt;
            direction = Dir::Down;
            return;
        } else if (key == EditorKey::ArrowRight || key == EditorKey::ArrowDown) {
            direction = Dir::Down;
        } else if (key == EditorKey::ArrowLeft || key == EditorKey::ArrowUp) {
            direction = Dir::Up;
        } else {
            last_match = std::nullopt;
            direction = Dir::Down;
        }

        if (!last_match.has_value())
            direction = Dir::Down;
        int current = last_match.value_or(-1);

        for (int i = 0; i < E.numrows; i++) {
            current += (direction == Dir::Down ? 1 : -1);
            if (current == -1)
                current = E.numrows - 1;
            else if (current == E.numrows)
                current = 0;

            Erow &row = E.row[current];
            auto match = row.render.find(query);
            if (match != std::string::npos) {
                last_match = current;
                E.cy = current;
                E.cx = editor_row_rx_to_cx(row, match);
                E.rowoff = E.numrows;

                saved_hl = std::make_pair(current, row.hl);
                std::fill(row.hl.begin() + match, row.hl.begin() + match + query.size(),
                          EditorHighlight::Match);
                break;
            }
        }
    };

    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    auto query = editor_prompt("Search: %s (Use ESC/Arrows/Enter)", callback);

    if (!query.has_value()) {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** output ***/

void editor_scroll() {
    E.rx = (E.cy < E.numrows) ? editor_row_cx_to_rx(E.row[E.cy], E.cx) : 0;

    if (E.cy < E.rowoff)
        E.rowoff = E.cy;

    if (E.cy >= E.rowoff + E.screenrows)
        E.rowoff = E.cy - E.screenrows + 1;

    if (E.rx < E.coloff)
        E.coloff = E.rx;

    if (E.rx >= E.coloff + E.screencols)
        E.coloff = E.rx - E.screencols + 1;
}

void editor_draw_rows(std::string &buf) {
    for (int y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows / 3) {
                std::string welcome = "Kilo editor -- version " + KILO_VERSION;
                int padding = (E.screencols - int(welcome.size())) / 2;
                if (padding) {
                    buf.append("~");
                    padding--;
                }
                while (padding--)
                    buf.append(" ");
                buf.append(welcome.substr(0, std::min(int(welcome.size()), E.screencols)));
            } else {
                buf.append("~");
            }
        } else {
            int len = E.row[filerow].render.size() - E.coloff;
            if (len < 0)
                len = 0;
            if (len > E.screencols)
                len = E.screencols;
            // coloff ~ coloff+line
            std::string render(E.row[filerow].render.cbegin() + E.coloff,
                               E.row[filerow].render.cbegin() + E.coloff + len);
            std::vector<EditorHighlight> hl(E.row[filerow].hl.cbegin() + E.coloff,
                                            E.row[filerow].hl.cbegin() + E.coloff + len);
            std::optional<int> current_color;
            for (int j = 0; j < int(render.size()); j++) {
                std::string s{render[j]};
                if (std::iscntrl(render[j])) {
                    char sym = (render[j] <= 26) ? '@' + render[j] : '?';
                    buf += "\x1b[7m" + std::string{sym} + "\x1b[m";
                    if (current_color.has_value()) {
                        buf += "\x1b[" + std::to_string(current_color.value()) + "m";
                    }
                } else if (hl[j] == EditorHighlight::Normal) {
                    if (current_color.has_value()) {
                        buf += "\x1b[39m";
                        current_color.reset();
                    }
                    buf += s;
                } else {
                    int color = editor_syntax_to_color(hl[j]);
                    if (current_color != color) {
                        current_color = color;
                        buf += "\x1b[" + std::to_string(color) + "m";
                    }
                    buf += s;
                }
            }
            buf += "\x1b[39m";
        }

        buf.append("\x1b[K");
        buf.append("\r\n");
    }
}

void editor_draw_status_bar(std::string &buf) {
    buf.append("\x1b[7m");
    std::string filename = E.filename.has_value() ? E.filename.value() : "[No Name]";
    if (E.dirty)
        filename += "(modified)";
    std::string lstatus = filename.substr(0, 20) + " - " + std::to_string(E.numrows) + " lines";
    int len = lstatus.size();
    if (len > E.screencols)
        len = E.screencols;
    std::string rstatus = (E.syntax.has_value() ? E.syntax.value().filetype : "no ft") + " | " +
                          std::to_string(E.cy + 1) + "/" + std::to_string(E.numrows);
    int rlen = rstatus.size();
    buf.append(lstatus.substr(0, len));
    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            buf.append(rstatus);
            break;
        } else {
            buf.append(" ");
            len++;
        }
    }
    buf.append("\x1b[m");
    buf.append("\r\n");
}

void editor_draw_message_bar(std::string &buf) {
    buf.append("\x1b[K");
    int msglen = E.statusmsg.size();
    if (msglen > E.screencols)
        msglen = E.screencols;
    if (msglen && std::time(nullptr) - E.statusmsg_time < 5)
        buf.append(E.statusmsg.substr(0, msglen));
}

void editor_refresh_screen() {
    editor_scroll();

    std::string buf;

    buf.append("\x1b[?25l");
    buf.append("\x1b[H");

    editor_draw_rows(buf);
    editor_draw_status_bar(buf);
    editor_draw_message_bar(buf);

    int y = E.cy - E.rowoff + 1;
    int x = E.rx - E.coloff + 1;
    buf.append("\x1b[" + std::to_string(y) + ";" + std::to_string(x) + "H");

    buf.append("\x1b[?25h");

    write(STDOUT_FILENO, buf.c_str(), buf.size());
}

void editor_set_status_message(const char *fmt, ...) {
    char s[80];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(s, sizeof(s), fmt, ap);
    va_end(ap);
    E.statusmsg = s;
    E.statusmsg_time = std::time(nullptr);
}

/*** input ***/

std::optional<std::string>
editor_prompt(const std::string &prompt,
              const std::optional<std::function<void(const std::string &, int)>> &callback) {

    std::string buf;

    while (true) {
        editor_set_status_message(prompt.c_str(), buf.c_str());
        editor_refresh_screen();

        EditorKey key = editor_read_key();
        int c = static_cast<int>(key);
        if (key == EditorKey::DelKey || key == EditorKey::Backspace || c == CTRL_KEY('h')) {
            if (!buf.empty())
                buf.pop_back();
        } else if (c == '\x1b') {
            editor_set_status_message("");
            if (callback.has_value())
                callback.value()(buf, c);
            return std::nullopt;
        } else if (c == '\r') {
            if (!buf.empty()) {
                editor_set_status_message("");
                if (callback.has_value())
                    callback.value()(buf, c);
                return buf;
            }
        } else if (!std::iscntrl(c) && c < 128) {
            buf += c;
        }

        if (callback.has_value())
            callback.value()(buf, c);
    }

    return buf;
}

void editor_move_cursor(EditorKey key) {
    std::optional<std::string> row =
        (E.cy >= E.numrows) ? std::nullopt : std::make_optional(E.row[E.cy].chars);
    switch (key) {
    case EditorKey::ArrowLeft:
        if (E.cx != 0)
            E.cx--;
        else if (E.cy > 0) {
            E.cy--;
            E.cx = E.row[E.cy].chars.size();
        }
        break;
    case EditorKey::ArrowRight:
        if (row && E.cx < int(row.value().size()))
            E.cx++;
        else if (row && E.cx == int(row.value().size())) {
            E.cy++;
            E.cx = 0;
        }
        break;
    case EditorKey::ArrowUp:
        if (E.cy != 0)
            E.cy--;
        break;
    case EditorKey::ArrowDown:
        if (E.cy < E.numrows)
            E.cy++;
        break;
    default:
        break;
    }

    int rowlen = (E.cy >= E.numrows) ? 0 : E.row[E.cy].chars.size();
    if (E.cx > rowlen)
        E.cx = rowlen;
}

void editor_process_keypress() {
    static int quit_times = KILO_QUIT_TIMES;

    EditorKey key = editor_read_key();
    switch (key) {
    case EditorKey::ArrowUp:
    case EditorKey::ArrowDown:
    case EditorKey::ArrowLeft:
    case EditorKey::ArrowRight:
        editor_move_cursor(key);
        break;
    case EditorKey::PageUp:
        E.cy = E.rowoff;
        for (int _ = 0; _ < E.screenrows; _++)
            editor_move_cursor(EditorKey::ArrowUp);
        break;
    case EditorKey::PageDown:
        E.cy = E.rowoff + E.screenrows - 1;
        if (E.cy > E.numrows)
            E.cy = E.numrows;
        for (int _ = 0; _ < E.screenrows; _++)
            editor_move_cursor(EditorKey::ArrowDown);
        break;
    case EditorKey::HomeKey:
        E.cx = 0;
        break;
    case EditorKey::EndKey:
        if (E.cy < E.numrows)
            E.cx = E.row[E.cy].chars.size();
        break;
    case EditorKey::Backspace:
        editor_del_char();
        break;
    case EditorKey::DelKey:
        editor_move_cursor(EditorKey::ArrowRight);
        editor_del_char();
        break;
    default:
        int c = static_cast<int>(key);
        switch (c) {
        case '\r':
            editor_insert_newline();
            break;
        case CTRL_KEY('q'):
        case CTRL_KEY('w'):
            if (E.dirty && quit_times > 0) {
                editor_set_status_message("WARNING!!! File has unsaved changes. "
                                          "Press Ctrl-Q %d more times to quit.",
                                          quit_times);
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            std::exit(0);
            break;
        case CTRL_KEY('s'):
            editor_save();
            break;
        case CTRL_KEY('l'):
        case '\x1b':
            break;
        case CTRL_KEY('h'):
            editor_del_char();
            break;
        case CTRL_KEY('f'):
            editor_find();
            break;
        default:
            editor_insert_char(c);
            break;
        }
    }
}

/*** init ***/

void init_editor() {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.dirty = false;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.statusmsg_time = 0;
    auto ws = get_window_size();
    if (!ws.has_value())
        die("get_window_size");
    auto [row, col] = ws.value();
    // status-bar and status-message
    E.screenrows = row - 2;
    E.screencols = col;
}

int main(int argc, char *argv[]) {
    enable_raw_mode();
    init_editor();
    if (argc >= 2)
        editor_open(argv[1]);

    editor_set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (true) {
        editor_refresh_screen();
        editor_process_keypress();
    }
    return 0;
}
