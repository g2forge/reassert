package com.g2forge.reassert.core.api.scanner;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.alexandria.java.io.dataaccess.PathDataSource;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.file.Contains;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class LocalScannerProxy {
	@RequiredArgsConstructor
	protected class Item implements IScanner.IItem {
		@Getter
		protected final Path path;

		@Getter(lazy = true)
		private final IDataSource data = Files.isRegularFile(path) ? new PathDataSource(path) : null;

		@Override
		public ICoordinates getCoordinates() {
			return getCoordinateFactory().apply(getPath());
		}
	}

	protected final IScanner scanner;

	protected final IFunction1<? super Path, ? extends ICoordinates> coordinateFactory;

	public void scan(final Path root, IReassertGraphBuilder builder, final Artifact<?> artifact) {
		final List<IScanner.IItem> items = new ArrayList<>();
		try {
			Files.walkFileTree(root, new FileVisitor<Path>() {
				protected void item(final IScanner scanner, final List<IScanner.IItem> items, Path dir) {
					final IScanner.IItem item = new Item(dir);
					if (scanner.isRelevant(item)) items.add(item);
				}

				@Override
				public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
					return FileVisitResult.CONTINUE;
				}

				@Override
				public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
					item(scanner, items, dir);
					return FileVisitResult.CONTINUE;
				}

				@Override
				public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
					item(scanner, items, file);
					return FileVisitResult.CONTINUE;
				}

				@Override
				public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
					return FileVisitResult.CONTINUE;
				}
			});
		} catch (IOException exception) {
			throw new RuntimeIOException(exception);
		}
		scanner.load(items, artifact, new IReassertGraphBuilder() {
			@Override
			public IReassertGraphBuilder callback(IReassertGraphBuilder.ICallback<?> callback) {
				builder.callback(callback);
				return this;
			}

			@Override
			public IReassertGraphBuilder edge(IVertex source, IVertex target, IEdge edge) {
				builder.edge(source, target, edge);
				return this;
			}

			@Override
			public IReassertGraphBuilder vertex(IVertex vertex) {
				builder.vertex(vertex);
				if (vertex.isMaterial()) builder.edge(artifact, vertex, new Contains());
				return this;
			}
		});
	}
}
