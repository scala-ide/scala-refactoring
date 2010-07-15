package scala.tools.refactoring;

import java.awt.BorderLayout;
import java.awt.Event;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

public class EditorUi {

  private JFrame jFrame = null; // @jve:decl-index=0:visual-constraint="10,10"
  private JPanel jContentPane = null;
  private JMenuBar jJMenuBar = null;
  private JMenu fileMenu = null;
  private JMenu helpMenu = null;
  private JMenuItem exitMenuItem = null;
  private JMenuItem aboutMenuItem = null;
  private JMenuItem openMenuItem = null;
  private JDialog aboutDialog = null;
  private JPanel aboutContentPane = null;
  private JLabel aboutVersionLabel = null;
  private JMenu refactorMenu = null;
  private JMenuItem renameMenuItem = null;
  private JMenuItem organizeMenuItem = null;
  private JMenuItem extractLocalMenuItem = null;
  private JMenuItem extractMethodMenuItem = null;
  private JTextArea editorTextArea = null;
  private JScrollPane jScrollPane = null;
  /**
   * This method initializes refactorMenu
   * 
   * @return javax.swing.JMenu
   */
  private JMenu getRefactorMenu() {
    if (refactorMenu == null) {
      refactorMenu = new JMenu();
      refactorMenu.setText("Refactor");
      refactorMenu.add(getRenameMenuItem());
      refactorMenu.add(getOrganizeMenuItem());
      refactorMenu.add(getExtractMethodMenuItem());
      refactorMenu.add(getExtractLocalMenuItem());
    }
    return refactorMenu;
  }

  /**
   * This method initializes renameMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  public JMenuItem getRenameMenuItem() {
    if (renameMenuItem == null) {
      renameMenuItem = new JMenuItem();
      renameMenuItem.setText("Rename");
    }
    return renameMenuItem;
  }

  /**
   * This method initializes organizeMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  public JMenuItem getOrganizeMenuItem() {
    if (organizeMenuItem == null) {
      organizeMenuItem = new JMenuItem();
      organizeMenuItem.setText("Organize Imports");
    }
    return organizeMenuItem;
  }

  /**
   * This method initializes organizeMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  public JMenuItem getExtractLocalMenuItem() {
    if (extractLocalMenuItem == null) {
      extractLocalMenuItem = new JMenuItem();
      extractLocalMenuItem.setText("Extract Local");
    }
    return extractLocalMenuItem;
  }

  /**
   * This method initializes organizeMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  public JMenuItem getExtractMethodMenuItem() {
    if (extractMethodMenuItem == null) {
      extractMethodMenuItem = new JMenuItem();
      extractMethodMenuItem.setText("Extract Method");
    }
    return extractMethodMenuItem;
  }

  /**
   * This method initializes editorTextArea
   * 
   * @return javax.swing.JTextArea
   */
  public JTextArea getEditorTextArea() {
    if (editorTextArea == null) {
      editorTextArea = new JTextArea();
      editorTextArea.setFont(new Font("Monospaced", Font.PLAIN, 14));
    }
    return editorTextArea;
  }

  /**
   * This method initializes jScrollPane  
   *  
   * @return javax.swing.JScrollPane  
   */
  private JScrollPane getJScrollPane() {
    if (jScrollPane == null) {
      jScrollPane = new JScrollPane();
      jScrollPane.setViewportView(getEditorTextArea());
    }
    return jScrollPane;
  }

  /**
   * This method initializes jFrame
   * 
   * @return javax.swing.JFrame
   */
  public JFrame getJFrame() {
    if (jFrame == null) {
      jFrame = new JFrame();
      jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      jFrame.setJMenuBar(getJJMenuBar());
      jFrame.setSize(467, 323);
      jFrame.setContentPane(getJContentPane());
      jFrame.setTitle("Refactoring Demo Editor");
    }
    return jFrame;
  }

  /**
   * This method initializes jContentPane
   * 
   * @return javax.swing.JPanel
   */
  private JPanel getJContentPane() {
    if (jContentPane == null) {
      jContentPane = new JPanel();
      jContentPane.setLayout(new BorderLayout());
      jContentPane.add(getJScrollPane(), BorderLayout.CENTER);
    }
    return jContentPane;
  }

  /**
   * This method initializes jJMenuBar
   * 
   * @return javax.swing.JMenuBar
   */
  private JMenuBar getJJMenuBar() {
    if (jJMenuBar == null) {
      jJMenuBar = new JMenuBar();
      jJMenuBar.add(getFileMenu());
      jJMenuBar.add(getRefactorMenu());
      jJMenuBar.add(getHelpMenu());
    }
    return jJMenuBar;
  }

  /**
   * This method initializes jMenu
   * 
   * @return javax.swing.JMenu
   */
  private JMenu getFileMenu() {
    if (fileMenu == null) {
      fileMenu = new JMenu();
      fileMenu.setText("File");
      fileMenu.add(getOpenMenuItem());
      fileMenu.add(getExitMenuItem());
    }
    return fileMenu;
  }

  /**
   * This method initializes jMenu
   * 
   * @return javax.swing.JMenu
   */
  private JMenu getHelpMenu() {
    if (helpMenu == null) {
      helpMenu = new JMenu();
      helpMenu.setText("Help");
      helpMenu.add(getAboutMenuItem());
    }
    return helpMenu;
  }

  /**
   * This method initializes jMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  private JMenuItem getExitMenuItem() {
    if (exitMenuItem == null) {
      exitMenuItem = new JMenuItem();
      exitMenuItem.setText("Exit");
      exitMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          System.exit(0);
        }
      });
    }
    return exitMenuItem;
  }

  /**
   * This method initializes jMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  private JMenuItem getAboutMenuItem() {
    if (aboutMenuItem == null) {
      aboutMenuItem = new JMenuItem();
      aboutMenuItem.setText("About");
      aboutMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          JDialog aboutDialog = getAboutDialog();
          aboutDialog.pack();
          Point loc = getJFrame().getLocation();
          loc.translate(20, 20);
          aboutDialog.setLocation(loc);
          aboutDialog.setVisible(true);
        }
      });
    }
    return aboutMenuItem;
  }

  /**
   * This method initializes aboutDialog
   * 
   * @return javax.swing.JDialog
   */
  private JDialog getAboutDialog() {
    if (aboutDialog == null) {
      aboutDialog = new JDialog(getJFrame(), true);
      aboutDialog.setTitle("About");
      aboutDialog.setContentPane(getAboutContentPane());
    }
    return aboutDialog;
  }

  /**
   * This method initializes aboutContentPane
   * 
   * @return javax.swing.JPanel
   */
  private JPanel getAboutContentPane() {
    if (aboutContentPane == null) {
      aboutContentPane = new JPanel();
      aboutContentPane.setLayout(new BorderLayout());
      aboutContentPane.add(getAboutVersionLabel(), BorderLayout.CENTER);
    }
    return aboutContentPane;
  }

  /**
   * This method initializes aboutVersionLabel
   * 
   * @return javax.swing.JLabel
   */
  private JLabel getAboutVersionLabel() {
    if (aboutVersionLabel == null) {
      aboutVersionLabel = new JLabel();
      aboutVersionLabel.setText("Version 1.0");
      aboutVersionLabel.setHorizontalAlignment(SwingConstants.CENTER);
    }
    return aboutVersionLabel;
  }

  /**
   * This method initializes jMenuItem
   * 
   * @return javax.swing.JMenuItem
   */
  private JMenuItem getOpenMenuItem() {
    if (openMenuItem == null) {
      openMenuItem = new JMenuItem();
      openMenuItem.setText("Open");
      openMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
          Event.CTRL_MASK, true));
      openMenuItem.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent e) {
          final JFileChooser fc = new JFileChooser();
          int returnVal = fc.showOpenDialog(getJFrame());

          if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();

            StringBuilder text = new StringBuilder();
            Scanner scanner;
            try {
              scanner = new Scanner(file);
              try {
                while (scanner.hasNextLine()) {
                  text.append(scanner.nextLine() + "\n");
                }
              } finally {
                scanner.close();
              }
            } catch (FileNotFoundException e1) {
            }

            getEditorTextArea().setText(text.toString());
          }
        }
      });
    }
    return openMenuItem;
  }

}
