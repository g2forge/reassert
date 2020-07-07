package com.g2forge.reassert.reassert.algorithm;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

public interface IGraphVisitor extends IConsumer1<Graph<IVertex, IEdge>> {}
