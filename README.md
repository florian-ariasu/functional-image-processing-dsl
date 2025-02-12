# 🌌 Functional Images: A Journey Through Dimensional Manipulation

## 🚀 Overview
Welcome to a cutting-edge Haskell project that explores the potential of functional programming in the realm of image processing. This project evolves through three distinct stages, each adding new complexity and capability to create a versatile system for defining and manipulating 2D image regions using functional programming techniques.

> *"In the world of functional programming, every image is a function waiting to be transformed and rendered."*

## 🌟 Project Evolution

### 📘 Stage 1: Shallow Embeddings - Basic Region Representation
The journey begins with a foundational approach to represent 2D regions and transformations in a purely functional way. This stage introduces the concept of regions as functions, starting with simple geometric shapes and basic transformation operations:

```haskell
type Point = (Float, Float)
type Pointed a = Point -> a
type Region = Pointed Bool
type Transformation = Point -> Point
```

#### Key Features
- **Basic Functional Region Representation**
  - Regions are defined as functions that take a point and return a boolean value indicating if it belongs to the region.
  - Simple geometric shapes like circles and rectangles are defined.
  
- **Primitive Transformations**
  - Translation: Moving regions in the 2D plane.
  - Scaling: Changing the size of regions.
  - Composition: Combining multiple transformations into a single operation.

### 📗 Stage 2: Deep Embeddings - Region Compositions and Transformations
In this stage, the project evolves to include more advanced abstractions, specifically using an Abstract Syntax Tree (AST) to represent regions and transformations. This allows for complex compositions and efficient transformation management.

```haskell
data RegionAST
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement RegionAST
    | Union RegionAST RegionAST
    | Intersection RegionAST RegionAST
    | Transform TransformationAST RegionAST
    deriving (Show, Eq)
```

#### Advanced Features
- **AST-Based Region Representation**
  - Regions are now represented by more complex data structures (AST), enabling more flexible region definitions and manipulations.
  - Supports operations like complementing a region, combining regions (union), and applying transformations.
  
- **Complex Transformations**
  - Transformations are now abstracted as part of the AST, allowing for better composition and optimization.
  - Operations such as scaling, rotation, and translation are more easily composed into a unified transformation structure.

### 📕 Stage 3: Advanced Folding - Generic Region Manipulation
The final stage introduces generic folding mechanisms and type classes, enabling more powerful and reusable operations on regions and transformations. This stage focuses on generalizing region and transformation handling to allow more complex and type-safe operations.

```haskell
newtype RegionAST
    = R (RegionShape RegionAST)
    deriving (Eq)

newtype TransformationAST
    = T (TransformationShape TransformationAST)
    deriving (Eq)
```

#### Revolutionary Features
- **Generic Folding**
  - Introduces higher-order functions and custom type classes to manipulate regions and transformations generically.
  - Pattern matching is enhanced, and region transformations are composable through fold operations.

- **Enhanced Type Safety**
  - Strong type-checking ensures safety across complex transformations and region operations.
  - Functor instances and custom type class implementations allow for flexible, reusable abstractions.

## 🎯 Use Cases

### 1. Computer Graphics
- Generate and manipulate 2D geometric shapes.
- Apply transformations to create complex images and animations.
- Produce procedural art using functional abstractions.

### 2. Game Development
- Dynamic boundary and collision detection systems.
- Procedural generation of environments and levels.
- Real-time image transformations and rendering.

### 3. Scientific Visualization
- Data region mapping for scientific data visualization.
- Transform coordinate systems for different data sets.
- Visualize patterns and structures in large data sets.

### 4. Educational Tools
- Demonstrate geometric concepts interactively.
- Provide visualizations for transformation operations.
- Create tools for learning functional programming and image processing.

## 🚀 Getting Started

### Prerequisites
- GHC (Glasgow Haskell Compiler)
- Basic understanding of functional programming
- Familiarity with geometric concepts

### Installation

#### If you have SSH keys set up:
```bash
git clone git@github.com:florian-ariasu/functional-image-dsl.git
```

#### If you are cloning using HTTPS:
```bash
git clone https://github.com/florian-ariasu/functional-image-dsl.git
```

#### Then, run the following commands:
```bash
cd functional-image-dsl
cd src
cd 1st-stage
ghci
:l TestShallow.hs
main
```

---

> **[!IMPORTANT]**  
> Ensure that you have **GHC** (Glasgow Haskell Compiler) installed on your machine.

## 🤝 Contributing
Contributions are welcome! Here's how you can help:
- Feature implementation
- Documentation improvements
- Test case additions
- Bug reports and fixes
