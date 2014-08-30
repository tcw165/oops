package mvc.v;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SpringLayout.Constraints;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import mvc.m.SimpleModel;

public class ProjectView extends JPanel implements ProjectObserver
{
  // Constants
  private static final int            COMMON_H       = 30;
  private static final int            COMMON_W       = 80;
  private static final int            PREFERED_W     = 500;
  private static final int            PREFERED_H     = 300;
  
  protected JPanel                    mThis          = this;
  
  protected static final JFileChooser mFoldDialog    = new JFileChooser();
  
  private SpringLayout                mLayout        = new SpringLayout();
  
  // Toolchain components.
  private JLabel                      mLbToolchain   = new JLabel(
                                                       "Toolchain :",
                                                       SwingConstants.RIGHT);
  private JTextField                  mTfToolchain   = new JTextField();
  private JButton                     mBtnToolchain  = new JButton("Browse");
  
  // Project components.
  private JLabel                      mLbProject     = new JLabel(
                                                       "Project :",
                                                       SwingConstants.RIGHT);
  private JTextField                  mTfProject     = new JTextField();
  private JButton                     mBtnProject    = new JButton("Browse");
  
  // Command components.
  private JPanel                      mCmdPanel      = new JPanel(
                                                       new SpringLayout());
  private JButton                     mBtnPlus       = new JButton("+");
  private JButton                     mBtnMinus      = new JButton("-");
  private JButton                     mBtnEdit       = new JButton("Edit");
  private JButton                     mBtnMoveUp     = new JButton("Up");
  private JButton                     mBtnMoveDown   = new JButton("Down");
  private JScrollPane                 mCmdScrollPane = new JScrollPane();
  private JTable                      mCmds          = new JTable();
  
  // Run components.
  private JButton                     mBtnRun        = new JButton("Run");
  
  public ProjectView()
  {
    setLayout(mLayout);
    InitComponent();
    InitListener();
  }
  
  private void InitComponent()
  {
    add(mLbToolchain);
    add(mTfToolchain);
    add(mBtnToolchain);
    
    add(mLbProject);
    add(mTfProject);
    add(mBtnProject);
    
    add(mCmdPanel);
    mCmdPanel.setBorder(BorderFactory.createTitledBorder(null, "Command",
      TitledBorder.CENTER, TitledBorder.TOP));
    
    // mCmds.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    mCmds.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
    mCmds.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    mCmdScrollPane.setViewportView(mCmds);
    
    mCmdPanel.add(mCmdScrollPane);
    mCmdPanel.add(mBtnPlus);
    mCmdPanel.add(mBtnMinus);
    mCmdPanel.add(mBtnEdit);
    mCmdPanel.add(mBtnMoveUp);
    mCmdPanel.add(mBtnMoveDown);
    
    mBtnMoveDown.setEnabled(false);
    mBtnMoveUp.setEnabled(false);
    mBtnEdit.setEnabled(false);
    
    add(mBtnRun);
    
    // Customize constraints.
    MakeConstraints();
    
    // ========================================================================
    
    mFoldDialog.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
  }
  
  private void InitListener()
  {
    mTfToolchain.addKeyListener(new TextfieldListener());
    mTfProject.addKeyListener(new TextfieldListener());
    
    mBtnToolchain.addActionListener(new BrowseFoldListener());
    mBtnProject.addActionListener(new BrowseFoldListener());
    
    mBtnPlus.addActionListener(new AddCommandListener());
    mBtnMinus.addActionListener(new RemoveCommandListener());
    mBtnMoveUp.addActionListener(new MoveCommandListener());
    mBtnMoveDown.addActionListener(new MoveCommandListener());
    mBtnEdit.addActionListener(new EditCommandListener());
  }
  
  private void MakeConstraints()
  {
    // Whole layout
    Spring TotalW = Spring.constant(0, PREFERED_W, Integer.MAX_VALUE);
    Spring TotalH = Spring.constant(0, PREFERED_H, Integer.MAX_VALUE);
    Constraints CstPanel = mLayout.getConstraints(this);
    CstPanel.setConstraint(SpringLayout.EAST, TotalW);
    CstPanel.setConstraint(SpringLayout.SOUTH, TotalH);
    
    // Toolchain and Project components.
    JComponent Array1[][] = {
      {mLbToolchain, mTfToolchain, mBtnToolchain},
      {mLbProject, mTfProject, mBtnProject}
    };
    // X direction.
    Spring X = Spring.constant(5);
    Spring CommonWidth = Spring.constant(COMMON_W);
    Spring VPad = Spring.constant(5);
    for (int Col = 0; Col < 3; ++Col)
    {
      for (int Row = 0; Row < 2; ++Row)
      {
        Constraints Cst = mLayout.getConstraints(Array1[Row][Col]);
        
        if (Col == 0)
        {
          Cst.setX(X);
          Cst.setWidth(CommonWidth);
        }
        else if (Col == 1)
        {
          Spring EastSpring = Spring.sum(CommonWidth, Spring.sum(VPad, VPad));
          
          Cst.setX(X);
          Cst.setConstraint(
            SpringLayout.EAST,
            Spring.sum(
              CstPanel.getConstraint(SpringLayout.EAST),
              Spring.minus(EastSpring)));
        }
        else if (Col == 2)
        {
          Cst.setWidth(CommonWidth);
          Cst.setConstraint(
            SpringLayout.EAST,
            Spring.sum(
              CstPanel.getConstraint(SpringLayout.EAST),
              Spring.minus(VPad)));
        }
      }
      
      X = Spring.sum(X, Spring.sum(CommonWidth, VPad));
    }
    // Y direction.
    Spring Y = Spring.constant(5);
    Spring CommonHeight = Spring.constant(COMMON_H);
    Spring HPad = Spring.constant(5);
    for (int Row = 0; Row < 2; ++Row)
    {
      for (int Col = 0; Col < 3; ++Col)
      {
        Constraints Cst = mLayout.getConstraints(Array1[Row][Col]);
        Cst.setY(Y);
        Cst.setHeight(CommonHeight);
      }
      Y = Spring.sum(Y, Spring.sum(CommonHeight, HPad));
    }
    
    // Run button.
    Constraints CstSome = mLayout.getConstraints(mBtnRun);
    CstSome.setWidth(CommonWidth);
    CstSome.setHeight(CommonHeight);
    CstSome.setConstraint(
      SpringLayout.EAST,
      Spring.sum(
        CstPanel.getConstraint(SpringLayout.EAST),
        Spring.minus(VPad)
        ));
    CstSome.setConstraint(
      SpringLayout.SOUTH,
      Spring.sum(
        CstPanel.getConstraint(SpringLayout.SOUTH),
        Spring.minus(HPad)
        ));
    
    // Command panel.
    SpringLayout CmdPanelLayout = (SpringLayout) mCmdPanel.getLayout();
    Constraints CstCmdPanel = CmdPanelLayout.getConstraints(mCmdPanel);
    CstCmdPanel.setWidth(Spring.constant(0, Integer.MAX_VALUE,
      Integer.MAX_VALUE));
    CstCmdPanel.setHeight(Spring.constant(0, Integer.MAX_VALUE,
      Integer.MAX_VALUE));
    
    CstSome = mLayout.getConstraints(mCmdPanel);;
    CstSome.setX(Spring.constant(5));
    CstSome.setY(Spring.constant(80));
    CstSome.setConstraint(
      SpringLayout.EAST,
      Spring.sum(
        CstPanel.getConstraint(SpringLayout.EAST),
        Spring.minus(VPad)
        ));
    CstSome.setConstraint(
      SpringLayout.SOUTH,
      Spring.sum(
        CstPanel.getConstraint(SpringLayout.SOUTH),
        Spring.minus(Spring.sum(CommonHeight, Spring.scale(HPad, 2)))
        ));
    
    CstSome = CmdPanelLayout.getConstraints(mBtnMinus);
    CstSome.setY(Spring.constant(5));
    CstSome.setWidth(Spring.constant(40));
    CstSome.setHeight(CommonHeight);
    CstSome.setConstraint(
      SpringLayout.EAST,
      Spring.sum(
        CstCmdPanel.getConstraint(SpringLayout.EAST),
        Spring.constant(-40)
        ));
    
    CstSome = CmdPanelLayout.getConstraints(mBtnPlus);
    CstSome.setY(Spring.constant(5));
    CstSome.setWidth(Spring.constant(40));
    CstSome.setHeight(CommonHeight);
    CstSome.setConstraint(
      SpringLayout.EAST,
      Spring.sum(
        CstCmdPanel.getConstraint(SpringLayout.EAST),
        Spring.constant(0)
        ));
    
    CstSome = CmdPanelLayout.getConstraints(mBtnEdit);
    CstSome.setY(Spring.constant(40));
    CstSome.setWidth(CommonWidth);
    CstSome.setHeight(CommonHeight);
    CstSome.setConstraint(
      SpringLayout.EAST,
      CstCmdPanel.getConstraint(SpringLayout.EAST)
      );
    
    CstSome = CmdPanelLayout.getConstraints(mBtnMoveUp);
    CstSome.setY(Spring.constant(75));
    CstSome.setWidth(CommonWidth);
    CstSome.setHeight(CommonHeight);
    CstSome.setConstraint(
      SpringLayout.EAST,
      CstCmdPanel.getConstraint(SpringLayout.EAST)
      );
    
    CstSome = CmdPanelLayout.getConstraints(mBtnMoveDown);
    CstSome.setY(Spring.constant(110));
    CstSome.setWidth(CommonWidth);
    CstSome.setHeight(CommonHeight);
    CstSome.setConstraint(
      SpringLayout.EAST,
      CstCmdPanel.getConstraint(SpringLayout.EAST)
      );
    
    CstSome = CmdPanelLayout.getConstraints(mCmdScrollPane);
    CstSome.setX(Spring.constant(10));
    CstSome.setY(Spring.constant(10));
    CstSome.setConstraint(
      SpringLayout.EAST,
      Spring.sum(
        CstCmdPanel.getConstraint(SpringLayout.EAST),
        Spring.constant(-90)
        ));
    CstSome.setConstraint(
      SpringLayout.SOUTH,
      Spring.sum(
        CstCmdPanel.getConstraint(SpringLayout.SOUTH),
        Spring.constant(-10)
        ));
  }
  
  private SimpleModel Model()
  {
    return SimpleModel.Api();
  }
  
  public void SetCommand(DefaultTableModel Command)
  {
    if (Command == null)
      return;
    
    mCmds.setModel(Command);
    mCmds.getModel().addTableModelListener(new CommandChangeListener());
  }
  
  public void SetProject(String Path)
  {
    mTfProject.setText(Path);
  }
  
  public void SetToolchain(String Path)
  {
    mTfToolchain.setText(Path);
  }
  
  @Override
  public void UpdateCommand(int Index)
  {
    mCmds.updateUI();
    
    // Edit button.
    if (Model().ProjectCommand().getRowCount() > 0)
    {
      mBtnEdit.setEnabled(true);
    }
    else
    {
      mBtnEdit.setEnabled(false);
    }
    
    // Button up and down.
    if (Model().ProjectCommand().getRowCount() > 1)
    {
      mBtnMoveDown.setEnabled(true);
      mBtnMoveUp.setEnabled(true);
    }
    else
    {
      mBtnMoveDown.setEnabled(false);
      mBtnMoveUp.setEnabled(false);
    }
    
    if (Index >= 0)
    {
      mCmds.changeSelection(Index, 0, false, false);
    }
  }
  
  @Override
  public void UpdateProject(String Path)
  {
    JTabbedPane Parent = (JTabbedPane) getParent();
    
    if (Parent.getSelectedComponent().equals(mThis))
      SetProject(Path);
  }
  
  @Override
  public void UpdateToolchain(String Path)
  {
    JTabbedPane Parent = (JTabbedPane) getParent();
    
    if (Parent.getSelectedComponent().equals(mThis))
      SetToolchain(Path);
  }
  
  /**
   * ##########################################################################
   * ####################### [ Friendly listener class ] ######################
   * ##########################################################################
   */
  
  private class AddCommandListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      Model().AddCommand("New");
    }
  }
  
  private class BrowseFoldListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      int Ret = mFoldDialog.showOpenDialog(SwingUtilities
        .getWindowAncestor(mThis));
      
      if (Ret != JFileChooser.APPROVE_OPTION)
        return;
      
      Component Btn = (JButton) Event.getSource();
      String Path = mFoldDialog.getSelectedFile().toString();
      
      if (mBtnToolchain.equals(Btn))
        Model().SetToolchainPath(Path);
      else if (mBtnProject.equals(Btn))
        Model().SetProjectPath(Path);
    }
  }
  
  private class CommandChangeListener implements TableModelListener
  {
    @Override
    public void tableChanged(TableModelEvent Event)
    {
      if (Event.getType() == TableModelEvent.UPDATE)
      {
        int FirstRow = Event.getFirstRow();
        if (((String) Model().ProjectCommand().getValueAt(FirstRow, 0)).length() == 0)
        {
          Model().RemoveCommand(FirstRow);
        }
      }
      
      // AdjustColumnWidth();
    }
    
    // private void AdjustColumnWidth()
    // {
    // int PreferredWidth = 0;
    // int MaxWidth = mCmds.getColumnModel().getColumn(0).getMaxWidth();
    //
    // for (int Row = 0; Row < mCmds.getRowCount(); Row++)
    // {
    // PreferredWidth = Math.max(PreferredWidth, GetCellDataWidth(Row, 0));
    // if (PreferredWidth >= MaxWidth)
    // break;
    // }
    //
    // PreferredWidth = Math.max(PreferredWidth,
    // mCmdScrollPane.getVisibleRect().width);
    //
    // TableColumn Col = mCmds.getColumnModel().getColumn(0);
    // mCmds.getTableHeader().setResizingColumn(Col);
    // Col.setWidth(PreferredWidth);
    // }
    //
    // private int GetCellDataWidth(int Row, int Col)
    // {
    // TableCellRenderer cellRenderer = mCmds.getCellRenderer(Row, Col);
    // Component Comp = mCmds.prepareRenderer(cellRenderer, Row, Col);
    //
    // return Comp.getPreferredSize().width + mCmds.getIntercellSpacing().width;
    // }
  }
  
  private class EditCommandListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      if (mCmds.getSelectedRow() < 0)
        return;
      
      int Index = mCmds.getSelectedRow();
      mCmds.changeSelection(Index, 0, false, false);
      mCmds.editCellAt(Index, 0);
      mCmds.getEditorComponent().requestFocusInWindow();
    }
  }
  
  private class MoveCommandListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      Component Comp = (Component) Event.getSource();
      
      if (Comp.equals(mBtnMoveDown))
      {
        if (mCmds.getSelectedRow() + 1 < mCmds.getRowCount())
          Model().SwapCommand(
            mCmds.getSelectedRow(),
            mCmds.getSelectedRow() + 1);
      }
      else
      {
        if (mCmds.getSelectedRow() > 0)
          Model().SwapCommand(
            mCmds.getSelectedRow(),
            mCmds.getSelectedRow() - 1);
      }
    }
  }
  
  private class RemoveCommandListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent Event)
    {
      if (mCmds.getSelectedRow() >= 0 &&
        mCmds.getSelectedRow() < mCmds.getRowCount())
      {
        Model().RemoveCommand(mCmds.getSelectedRow());
      }
    }
  }
  
  private class TextfieldListener implements KeyListener
  {
    
    @Override
    public void keyPressed(KeyEvent Event)
    {
      // getParent()
    }
    
    @Override
    public void keyReleased(KeyEvent Event)
    {
      JTabbedPane Parent = (JTabbedPane) getParent();
      
      if (Parent.getSelectedComponent().equals(mThis))
      {
        Component Comp = Event.getComponent();
        
        if (Comp.equals(mTfToolchain))
        {
          Model().SetToolchainPath(mTfToolchain.getText());
        }
        else if (Comp.equals(mTfProject))
        {
          Model().SetProjectPath(mTfProject.getText());
        }
      }
    }
    
    @Override
    public void keyTyped(KeyEvent Event)
    {
    }
  }
  
}
