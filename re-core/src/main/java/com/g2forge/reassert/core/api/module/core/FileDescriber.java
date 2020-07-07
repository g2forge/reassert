package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.file.File;

import lombok.Getter;

public class FileDescriber implements IDescriber<File>, ISingleton {
	protected static final FileDescriber INSTANCE = new FileDescriber();

	public static FileDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<File> type = ITypeRef.of(File.class);

	protected FileDescriber() {}

	@Override
	public IDescription describe(File value) {
		return describeTyped(value.getCoordinates());
	}

	protected <Coordinates extends ICoordinates> IDescription describeTyped(Coordinates value) {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		final ISystem<Coordinates> system = (ISystem) value.getSystem();
		final IDescription identified = system.getCoordinateDescriber().describe(value);
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return identified.getIdentifier() + " " + File.class.getSimpleName().toLowerCase();
			}

			@Override
			public String getName() {
				return "File\n" + identified.getName();
			}
		};
	}

}