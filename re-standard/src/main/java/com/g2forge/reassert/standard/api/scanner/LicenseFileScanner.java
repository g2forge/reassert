package com.g2forge.reassert.standard.api.scanner;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;

import com.g2forge.alexandria.java.io.HTextIO;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.coordinates.Coordinates;
import com.g2forge.reassert.core.model.file.File;
import com.g2forge.reassert.core.model.file.Parsed;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class LicenseFileScanner implements IScanner {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	protected final IContext context;

	@ReassertLegalOpinion
	@Override
	public boolean isRelevant(IScanner.IItem item) {
		return item.getPath().getFileName().toString().toLowerCase().equals("license");
	}

	@Override
	public void load(Collection<IScanner.IItem> items, Artifact<?> container, IReassertGraphBuilder builder) {
		final ILicenseParser licenseParser = getContext().getLicenseParser();
		for (IScanner.IItem item : items) {
			try (final InputStream input = item.getData().getStream(ITypeRef.of(InputStream.class))) {
				final File file = new File(item.getCoordinates());
				builder.vertex(file).vertex(item.getCoordinates()).edge(file, item.getCoordinates(), new Coordinates());

				final ILicense license = licenseParser.parse(HTextIO.readAll(input, true));
				builder.vertex(license).edge(file, license, new Parsed());
				builder.edge(container, license, new Notice());
			} catch (IOException exception) {
				throw new RuntimeIOException(exception);
			}
		}
	}
}