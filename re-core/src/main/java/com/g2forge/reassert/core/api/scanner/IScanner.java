package com.g2forge.reassert.core.api.scanner;

import java.nio.file.Path;
import java.util.Collection;

import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

public interface IScanner {
	public interface IItem {
		public ICoordinates getCoordinates();
		
		public IDataSource getData();

		public Path getPath();
	}

	public boolean isRelevant(IItem item);

	public void load(Collection<IScanner.IItem> items, Artifact<?> container, IReassertGraphBuilder builder);
}
