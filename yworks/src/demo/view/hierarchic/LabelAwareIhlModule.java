package demo.view.hierarchic;


import y.module.LayoutModule;
import y.view.Graph2D;
import y.view.hierarchy.GroupLayoutConfigurator;
import y.layout.Layouter;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.HierarchicLayouter;
import y.layout.hierarchic.incremental.DrawingDistanceCalculator;
import y.layout.hierarchic.incremental.SimplexNodePlacer;
import y.option.OptionHandler;


/**
 * <code>LayoutModule</code> that launches
 * <code>IncrementalHierarchicLayouter</code>. The layouter is configured to use
 * <code>AdaptNodeToLabelWidths</code> and
 * <code>LabelAwareDrawingDistanceCalculator</code>.
 * 
 * @author Thomas Behr
 */
public class LabelAwareIhlModule extends LayoutModule {
  private static final String OPTION_PREPROCESS_GROUP_NODES_ONLY =
          "PREPROCESS_GROUP_NODES_ONLY";
  private static final String OPTION_PREPROCESS_GROUP_LABEL_WIDTH_ADJUSTMENT =
          "PREPROCESS_GROUP_LABEL_WIDTH_ADJUSTMENT";
  private static final String OPTION_INTERNAL_GROUP_LABEL_WIDTH_ADJUSTMENT =
          "INTERNAL_GROUP_LABEL_WIDTH_ADJUSTMENT";
  private static final String OPTION_USE_HORIZONTAL_GROUP_COMPACTION =
          "USE_HORIZONTAL_GROUP_COMPACTION";


  public LabelAwareIhlModule() {
    super("LABEL_AWARE_IHL_MODULE", "Thomas Behr", "");
  }

  protected OptionHandler createOptionHandler() {
    final OptionHandler options = new OptionHandler(getModuleName());
    options.addBool(OPTION_PREPROCESS_GROUP_NODES_ONLY, false);
    options.addDouble(OPTION_PREPROCESS_GROUP_LABEL_WIDTH_ADJUSTMENT, 20);
    options.addDouble(OPTION_INTERNAL_GROUP_LABEL_WIDTH_ADJUSTMENT, 0);
    options.addBool(OPTION_USE_HORIZONTAL_GROUP_COMPACTION, true);
    return options;
  }

  protected void mainrun() {
    final Graph2D graph = getGraph2D();
    final GroupLayoutConfigurator glc = new GroupLayoutConfigurator(graph);
    glc.prepareAll();
    try {
      launchLayouter(createLayouter());
    } finally {
      glc.restoreAll();
    }
  }

  private Layouter createLayouter() {
    final OptionHandler options = getOptionHandler();

    // layout stage that recalculates node bounds such that node.x is less than
    // or equal to the minimum x-coordinate of all node labels and
    // node.x + node.width is greater than or equal to the maximum
    // x-coordinate of all node labels
    final AdaptNodeToLabelWidths stage = new AdaptNodeToLabelWidths();
    stage.setAdaptGroupNodesOnly(
            options.getBool(OPTION_PREPROCESS_GROUP_NODES_ONLY));
    stage.setGroupLabelWidthAdjustment(
            options.getDouble(OPTION_PREPROCESS_GROUP_LABEL_WIDTH_ADJUSTMENT));

    final IncrementalHierarchicLayouter ihl =
            new IncrementalHierarchicLayouter();
    ihl.prependStage(stage);

    final HierarchicLayouter hl = ihl.getHierarchicLayouter();
    final DrawingDistanceCalculator ddc = hl.getDrawingDistanceCalculator();
    // drawing distance calculator that calculates a minimum width for all
    // non-empty group nodes in such a way that node.x is less than or equal to
    // the minimum x-coordinate of all node labels and node.x + node.width is
    // greater than or equal to the maximum x-coordinate of all node labels
    //
    // this is done due to the fact that non-empty group nodes are an exception
    // to the general "no node resizing" policy of yFiles layout algorithms
    final LabelAwareDrawingDistanceCalculator laddc =
            new LabelAwareDrawingDistanceCalculator(ddc);
    laddc.setGroupLabelWidthAdjustment(
            options.getDouble(OPTION_INTERNAL_GROUP_LABEL_WIDTH_ADJUSTMENT));
    hl.setDrawingDistanceCalculator(laddc);

    // horizontal group compaction tries to prevent
    // IncrementalHierarchicLayouter from being to generous when calculating
    // group node sizes for non-empty group nodes
    if (options.getBool(OPTION_USE_HORIZONTAL_GROUP_COMPACTION)) {
      final SimplexNodePlacer snp = new SimplexNodePlacer();
      snp.setGroupCompactionStrategy(SimplexNodePlacer.GROUP_COMPACTION_MAX);
      ihl.setNodePlacer(snp);
    }

    return ihl;
  }
}
