package mvc.v;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.sql.Connection;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableModel;

import mvc.m.SimpleModel;

public class MainView extends JFrame implements WorkspaceObserver
{
  protected JFrame    mFrame                = this;
  
  // menu - file
  protected JMenuItem mQuitMenu             = new JMenuItem("Quit");
  protected JMenuItem mExportWorkspaceMenu  = new JMenuItem("Save Workspace");
  protected JMenuItem mImportWorkspaceMenu  = new JMenuItem("Load Workspace");
  
  // menu - edit
  protected JMenuItem mNewProjectMenu       = new JMenuItem("New");
  protected JMenuItem mDeleteProjectMenu    = new JMenuItem("Delete");
  protected JMenuItem mDuplicateProjectMenu = new JMenuItem("Duplicate");
  
  // menu - edit
  protected JMenuItem mAboutMenu            = new JMenuItem("About");
  protected JMenuItem mTutorialMenu         = new JMenuItem("Help");
  
  // Tab
  private JTabbedPane mTabbedPanel          = new JTabbedPane();
  
  public MainView()
  {
    InitComp();
    InitListener();
    
    setTitle("EDK2 Builder");
    setLocationRelativeTo(null);
    setDefaultCloseOperation(EXIT_ON_CLOSE);
    
    setPreferredSize(new Dimension(600, 400));
    setMinimumSize(getPreferredSize());
    pack();
  }
  
  @Override
  public void AddProjectView(String Name, String Toolchain, String Project, DefaultTableModel Command)
  {
    ProjectView Prj = new ProjectView();
    mTabbedPanel.addTab(Name, Prj);
    
    Prj.SetToolchain(Toolchain);
    Prj.SetProject(Project);
    Prj.SetCommand(Command);
    
    // Register as an observer.
    Model().RegisterProjectObserver(Prj);
    
    // menu
    mDuplicateProjectMenu.setEnabled(true);
    mDeleteProjectMenu.setEnabled(true);
  }
  
  @Override
  public void RemoveProjectView(int Index)
  {
    // Unregister as an observer.
    Model().UnRegisterProjectObserver(
      (ProjectView) mTabbedPanel.getSelectedComponent());
    
    mTabbedPanel.remove(mTabbedPanel.getSelectedIndex());
    
    // menu
    if (mTabbedPanel.getTabCount() == 0)
    {
      mDuplicateProjectMenu.setEnabled(false);
      mDeleteProjectMenu.setEnabled(false);
    }
  }
  
  private void InitComp()
  {
    JMenuBar MenuBar = new JMenuBar();
    
    JMenu FileMenu = new JMenu("File");
    FileMenu.add(mExportWorkspaceMenu);
    FileMenu.add(mImportWorkspaceMenu);
    FileMenu.addSeparator();
    FileMenu.add(mQuitMenu);
    
    // ========================================================================
    
    JMenu ProjectMenu = new JMenu("Project");
    
    mDeleteProjectMenu.setEnabled(false);
    mDuplicateProjectMenu.setEnabled(false);
    
    ProjectMenu.add(mNewProjectMenu);
    ProjectMenu.add(mDuplicateProjectMenu);
    ProjectMenu.add(mDeleteProjectMenu);
    
    // ========================================================================
    
    JMenu HelpMenu = new JMenu("Help");
    HelpMenu.add(mTutorialMenu);
    HelpMenu.addSeparator();
    HelpMenu.add(mAboutMenu);
    
    // JMenuBar add JMenu
    MenuBar.add(FileMenu);
    MenuBar.add(ProjectMenu);
    MenuBar.add(HelpMenu);
    
    // ========================================================================
    
    // Set JMenuBar
    setJMenuBar(MenuBar);
    
    // ========================================================================
    
    mTabbedPanel.addChangeListener(new TabChangeListener());
    add(mTabbedPanel);
  }
  
  private void InitListener()
  {
    Model().RegisterWorkspaceObserver(this);
    
    mQuitMenu.addActionListener(new ExitListener());
    
    mExportWorkspaceMenu.addActionListener(new ExportWorksapceistener());
    
    mNewProjectMenu.addActionListener(new AddProjectListener());
    mDeleteProjectMenu.addActionListener(new RemoveProjectListener());
    mDuplicateProjectMenu.addActionListener(new DuplicateProjectListener());
    
    mTutorialMenu.addActionListener(new HelpListener());
  }
  
  private SimpleModel Model()
  {
    return SimpleModel.Api();
  }
  
  /**
   * ##########################################################################
   * ####################### [ Friendly listener class ] ######################
   * ##########################################################################
   */
  
  private class AddProjectListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      String Str = JOptionPane.showInputDialog(mFrame,
        new String("Please enter you project name."));
      
      if (Str != null && Str.length() > 0)
        Model().AddProject(Str, "", "", null);
    }
  }
  
  private class DuplicateProjectListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      Model().DuplicateProject();
    }
  }
  
  private class ExitListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      System.exit(0);
    }
  }
  
  private class ExportWorksapceistener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
    }
  }
  
  private class HelpListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      Model().DumpAll();
    }
  }
  
  private class RemoveProjectListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      Model().RemoveProject();
    }
  }
  
  private class TabChangeListener implements ChangeListener
  {
    @Override
    public void stateChanged(ChangeEvent Event)
    {
      Model().SelectProject(mTabbedPanel.getSelectedIndex());
    }
  }
  
}
