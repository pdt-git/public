package pdt.y.graphml;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import org.junit.Before;
import org.junit.Test;

import y.base.Edge;
import y.base.Node;
import y.view.Graph2D;

public class GraphMLReaderTest {
	
    private static final String GraphMLHeader = "<?xml version='1.0' encoding='UTF-8'?>"
                                                 +"<graphml xmlns='http://graphml.graphdrawing.org/xmlns' "  
                                                 +"xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' "
                                                 +"xsi:schemaLocation='http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd'> <key id='id' for='node' attr.name='id' attr.type='string'/>";
    private static final String EMPTY_GRAPH = GraphMLHeader+"<graph></graph></graphml>";
    private static final String GRAPH_WITH_TWO_NODES_AND_A_EDGE = GraphMLHeader+"<graph><node id='1'><data key='id'>1</data></node><node id='2'><data key='id'>2</data></node><edge source='1' target='2'/></graph></graphml>";
	
	
	private GraphMLReader graphReader;

	private InputStream convertToStream(String string) throws UnsupportedEncodingException{
		return new  ByteArrayInputStream(string.getBytes("UTF-8"));
	}
	
	@Before
	public void setup(){
		graphReader= new GraphMLReader();
	}
	@Test
	public void empty_graph() throws UnsupportedEncodingException{
		InputStream input= convertToStream(EMPTY_GRAPH);
		Graph2D graph=graphReader.readFile(input);
		assertThat(graph.isEmpty(),is(true));
	}
	
	@Test
	public void graph_contains_two_nodes_and_a_edge() throws UnsupportedEncodingException{
		InputStream input= convertToStream(GRAPH_WITH_TWO_NODES_AND_A_EDGE);
		Graph2D graph=graphReader.readFile(input);
		
		assertThat(graph.isEmpty(),is(false));
		assertThat(graph.nodeCount(),is(2));
		assertThat(graph.edgeCount(),is(1));
		
		Node firstNode = graph.firstNode();
		Node lastNode = graph.lastNode();
		assertThat(firstNode,is(not(equalTo(lastNode))));
		
		Edge edge = graph.firstEdge(); 
		assertThat(edge.source(),is(firstNode));
		assertThat(edge.target(),is(lastNode));
	}
}
